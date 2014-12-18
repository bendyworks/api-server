-- API PosgreSQL schema
-- =======================



-- required extensions

CREATE EXTENSION pgcrypto;
CREATE EXTENSION "uuid-ossp";
CREATE EXTENSION citext;



-- shared stored procedures

CREATE FUNCTION on_record_insert() RETURNS trigger AS $$
  DECLARE
    id_sequence VARCHAR;
  BEGIN
    SELECT TG_ARGV[0] INTO id_sequence;     -- the name of the ID sequence for this table
    NEW.id         := nextval(id_sequence); -- set the ID as the next sequence value
    NEW.created_at := now();
    NEW.updated_at := now();
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;

CREATE FUNCTION on_record_update() RETURNS trigger AS $$
  BEGIN
    NEW.id         := OLD.id;         -- ensure IDs aren't altered on updates
    NEW.created_at := OLD.created_at; -- ensure date-created isn't altered on updates
    NEW.updated_at := now();
    RETURN NEW;
  END;
$$ LANGUAGE plpgsql;



-- the Resource resource

CREATE TABLE resources (
  id           INTEGER    PRIMARY KEY,
  name         VARCHAR    NOT NULL,
  email        CITEXT     NOT NULL,
  optional     VARCHAR,
  created_at   TIMESTAMP  NOT NULL,
  updated_at   TIMESTAMP  NOT NULL
);

CREATE UNIQUE INDEX email_on_resources
  ON resources (email);

CREATE SEQUENCE resource_ids START 1;

CREATE TRIGGER resources_insert
  BEFORE INSERT ON resources
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('resource_ids');

CREATE TRIGGER resources_update
  BEFORE UPDATE ON resources
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();



-- the User resource

CREATE TABLE users (
  id           INTEGER    PRIMARY KEY,
  resource_id  INTEGER    REFERENCES resources ON DELETE RESTRICT,
  token_hash   VARCHAR    NOT NULL,
  created_at   TIMESTAMP  NOT NULL,
  updated_at   TIMESTAMP  NOT NULL
);

CREATE UNIQUE INDEX resource_id_on_users
  ON users (resource_id);

CREATE SEQUENCE user_ids START 1;

CREATE TRIGGER users_insert
  BEFORE INSERT ON users
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('user_ids');

CREATE TRIGGER users_update
  BEFORE UPDATE ON users
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();



-- the PendingUserResource resource

CREATE TABLE pending_user_resources (
  id             INTEGER    PRIMARY KEY,
  uuid           VARCHAR    NOT NULL,
  user_id        INTEGER    REFERENCES users NOT NULL,
  resource_email CITEXT     NOT NULL,
  created_at     TIMESTAMP  NOT NULL,
  updated_at     TIMESTAMP  NOT NULL
);

CREATE UNIQUE INDEX uuid_on_pending_user_resources
  ON pending_user_resources (uuid);

CREATE UNIQUE INDEX user_id_and_resource_email_on_pending_user_resources
  ON pending_user_resources (user_id, resource_email);

CREATE SEQUENCE pending_user_resource_ids START 1;

CREATE TRIGGER pending_user_resources_insert
  BEFORE INSERT ON pending_user_resources
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_insert('pending_user_resource_ids');

CREATE TRIGGER pending_user_resources_update
  BEFORE UPDATE ON pending_user_resources
  FOR EACH ROW
  EXECUTE PROCEDURE on_record_update();
