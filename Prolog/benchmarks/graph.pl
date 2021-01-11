%%%% -*- Mode: prolog -*-

:- dynamic graph/1, vertex/2, arc/4.

graph(gregor).

vertex(gregor, a).
vertex(gregor, b).
vertex(gregor, c).
vertex(gregor, d).
vertex(gregor, e).
vertex(gregor, f).
vertex(gregor, g).
vertex(gregor, h).
vertex(gregor, i).

arc(gregor, a, b, 4).
arc(gregor, a, h, 8).
arc(gregor, b, c, 8).
arc(gregor, b, h, 11).
arc(gregor, c, d, 7).
arc(gregor, c, f, 4).
arc(gregor, c, i, 2).
arc(gregor, d, e, 9).
arc(gregor, d, f, 14).
arc(gregor, e, f, 10).
arc(gregor, f, g, 2).
arc(gregor, g, h, 1).
arc(gregor, g, i, 6).
arc(gregor, h, i, 7).
