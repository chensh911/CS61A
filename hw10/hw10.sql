CREATE TABLE parents AS
  SELECT "abraham" AS parent, "barack" AS child UNION
  SELECT "abraham"          , "clinton"         UNION
  SELECT "delano"           , "herbert"         UNION
  SELECT "fillmore"         , "abraham"         UNION
  SELECT "fillmore"         , "delano"          UNION
  SELECT "fillmore"         , "grover"          UNION
  SELECT "eisenhower"       , "fillmore";

CREATE TABLE dogs AS
  SELECT "abraham" AS name, "long" AS fur, 26 AS height UNION
  SELECT "barack"         , "short"      , 52           UNION
  SELECT "clinton"        , "long"       , 47           UNION
  SELECT "delano"         , "long"       , 46           UNION
  SELECT "eisenhower"     , "short"      , 35           UNION
  SELECT "fillmore"       , "curly"      , 32           UNION
  SELECT "grover"         , "short"      , 28           UNION
  SELECT "herbert"        , "curly"      , 31;

CREATE TABLE sizes AS
  SELECT "toy" AS size, 24 AS min, 28 AS max UNION
  SELECT "mini"       , 28       , 35        UNION
  SELECT "medium"     , 35       , 45        UNION
  SELECT "standard"   , 45       , 60;

-------------------------------------------------------------
-- PLEASE DO NOT CHANGE ANY SQL STATEMENTS ABOVE THIS LINE --
-------------------------------------------------------------

-- The size of each dog
CREATE TABLE size_of_dogs AS
  SELECT a.name as name, b.size as size from dogs as a, sizes as b
  where a.height > b.min and a.height <= b.max;

-- All dogs with parents ordered by decreasing height of their parent
CREATE TABLE by_parent_height AS
  SELECT a.child from parents as a, dogs as b where a.parent = b.name order by -b.height;

-- Filling out this helper table is optional
CREATE TABLE siblings AS
  SELECT a.child as first, b.child as second, c.size as kind_f, d.size as kind_s 
  from parents as a, parents as b, size_of_dogs as c, size_of_dogs as d
  where a.parent = b.parent and a.child < b.child and c.name = a.child and d.name = b.child
  order by a.child;

-- Sentences about siblings that are the same size
CREATE TABLE sentences AS
  SELECT a.first || " and " || a.second || " are " || a.kind_f|| " siblings"
  from siblings as a where a.kind_f = a.kind_s;

-- Ways to stack 4 dogs to a height of at least 170, ordered by total height
CREATE TABLE stacks_helper(dogs, stack_height, last_height);

-- Add your INSERT INTOs here
insert into stacks_helper 
  select a.dogs || ", " || b.dogs, a.stack_height + b.last_height, b.last_height from 
  stacks_helper as a, stacks_helper as b where a.last_height < b.last_height group by a.stack_height;

insert into stacks_helper 
  select a.dogs || ", " || b.dogs, a.stack_height + b.last_height, b.last_height from stacks_helper as a,
  stacks_helper as b where a.stack_height > 52 and a.last_height < b.last_height and b.stack_height < 54;

insert into stacks_helper
  select a.dogs || ", " || b.dogs, a.stack_height + b.last_height, b.last_height 
  from stacks_helper as a, stacks_helper as b 
  where a.stack_height >= 85 and b.stack_height < 54 and a.last_height < b.last_height;


CREATE TABLE stacks AS
  SELECT dogs, stack_height from stacks_helper where stack_height > 170;