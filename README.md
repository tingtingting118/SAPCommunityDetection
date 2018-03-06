# SAPCommunityDetection
Analysis on the sub-network(10%) on SAP Community



#Files information 

1.“SAPSUB_edges.csv”

a.thread_author_id:the ID
of
a
user
who
posts
a
question
that
starts
a
new
thread
in
a
SAP
community
user
forum

b.message_author_id  :id).
The
left-
hand
column
represents
the
ID
of
a
user
who
provides
an
answer
to
the
posted
question


So,
a
directed
edge
from
the
left-­‐hand
node
to
a
right-­‐
hand
node
represents
an
answer
provided
to
a
question.
Since
a
user
can
answer
multiple
questions
posted
by
another
user
on
one
or
more
threads,
the
file
contains
duplicate
edge-­‐pairs,
which
can
be
combined
to
form
directed
edges
of
varying
weights



2.“SAPSUB_nodes.csv”
has
the
list
of
nodes
and
their
attributes.
User
ids
are
identified
by
“author_id”,
which
correspond
to
the
thread
and
message
authors
in
the
edges
file.
