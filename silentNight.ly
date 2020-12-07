\version "2.20.0"

#(set-global-staff-size 15.87)

melodyA = \relative c' {
  \key aes \major
  \time 3/4
  ees4. ( f8 ) ees4 |
  c2 r4 |
  ees4. ( f8 ) ees4 |
  c2 r4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  f2 f4 |
  aes4. ( g8 ) f4 |
  ees4. f8 ees4 |
  c2 r4 |
  f2 f4 |
  aes4. g8 f4 |
  ees4. f8 ees4 |
  c2 r4 |
  bes'2 bes4 |
  des4. bes8 g4 |
  aes2. ( |
  c2 ) r4 |
  aes4 ( ees4 ) c4 |
  ees4. des8 bes4 |
  aes2. ~ |
  aes2 r4 \bar "|."
}

vBaccA = \relative c' {
  \key aes \major
  \time 3/4
  R2.
  aes2 aes4 |
  aes2 r4 |
  aes2 aes4 |
  des2 r4 |
  ees2 des4 |
  c2 r4 |
  c2 c4 |
  des2 r4 |
  des2 des4 |
  c2 c4 |
  aes4. bes8 c4 |
  des2 r4 |
  des2 des4 |
  c2 c4 |
  aes4. bes8 c4 |
  des4. ( ees8 f4) |
  g4. (aes8) g4 |
  g4 f ees |
  d2. ( |
  ees2 ) r4 |
  bes4 (g4) g |
  f4. f8 g4 |
  ees2 r4 \bar "|."
}

vBaccCpt = \relative c' {
  \key aes \major
  \time 3/4
  R2.
  aes8 (bes c aes) ees' (des) |
  c2 r4 |
  aes8 (bes c aes) f' (ees) |
  des (ees f ees des c) |
  bes8 (c des bes) ees (des) |
  c (des ees des c bes) |
  aes (bes' aes ges) f (ees) |
  des8 (ees f ees des c) |
  bes8 (c des ees) des4 |
  des4 (c8 des) c (bes) |
  aes (bes c) des ees4  |
  des8 (ees des c bes aes) |
  bes8 (c des ees) des4 |
  des4 (c8 des) c (bes) |
  aes (bes c) des ees4  |
  ees8 (f g aes bes aes) |
  g (f ees4 ) des4 |
  c8 (des) ees4 c |
  des8 (ees f4 ees8 des) |
  c8 ( des c bes) aes4 |
  g8 (aes ) bes4 g8 (ees') |
  c (des ees des c bes |
  aes2) r4 \bar "|."
}

vBaccSimp = \relative c' {
  \key aes \major
  \time 3/4
  R2. |
  ees4. ( f8 ) ees4 |
  c2 r4 |
  ees4. ( f8 ) ees4 |
  des2 r4 |
  ees2 des4 |
  c2 r4 |
  c2 ees4 |
  des2 r4 |
  bes2 bes4 |
  c4. ( des8 ) c4 |
  aes4. bes8 c4 |
  des2 r4 |
  bes2 bes4 |
  c4. ( des8 ) c4 |
  aes4. bes8 c (des) |
  ees2 r4 |
  ees2 des4 |
  c4. des8 ees4 |
  aes2. ( |
  ees2 ) r4 |
  bes4 ( c4 ) des4 |
  c4. des8 c4 |
  aes2 r4 \bar "|."
}

vAaccB = \relative c' {
  \key aes \major
  \time 3/4
  R2. |
  c4 ( bes ) aes |
  aes2 r4 |
  c4 ( bes ) aes |
  bes4 ( ees ) r |
  ees2 des4 |
  c2 r4 |
  c2 c4 |
  des2 r4 |
  des2 des4 |
  c2 c4 |
  c4. des8 c4 |
  aes2 r4 |
  des2 des4 |
  c4 c c |
  c4. des8 c4 |
  aes2 r4 |
  ees'2 des4 |
  c4. des8 c4 |
  aes2. ( |
  c2 ) r4 |
  ees4 ( f ) ees |
  des4. c8 bes4 |
  aes2 r4 |
   \bar "|."
}

vAaccC = \relative c'' {
  \key aes \major
  \time 3/4
  R2. |
  g4 ( f ) ees |
  c2 r4 |
  g'4 ( f ) ees |
  des2 r4 |
  ees2 des4 |
  c2 r4 |
  f2 ees4 |
  des2 r4 |
  f4 (ees) des4 |
  c4  ( bes ) c4 |
  ees4. des8 c4 |
  des2 r4 |
  f2 des4 |
  c4 des c |
  ees4. des8 c4 |
  ees2 r4 |
  g4 (f) ees4 |
  des4 c bes4 |
  c2. ( |
  ees2 ) r4 |
  ees4 ( f ) g |
  aes4 ees des4 |
  c2 r4 |
   \bar "|."
}

vAaccD = \relative c'' {
  \key aes \major
  \time 3/4
  R2. |
  g4. ( aes8 ) g4 |
  ees2 r4 |
  g4. ( aes8 ) g4 |
  ees2 r4 |
  des'2 des4 |
  bes2 r4 |
  c2 c4 |
  g2 r4 |
  aes2 aes4 |
  c4. ( bes8 ) aes4 |
  g4. aes8 g4 |
  ees2 r4 |
  aes2 aes4 |
  c4. bes8 aes4 |
  g4. aes8 g4 |
  ees2 r4 |
  des'2 des4 |
  f4. ees8 des4 |
  c2. ( |
  ees2 ) r4 |
  c4 ( g4 ) ees4 |
  g4. f8 des4 |
  c2 r4 \bar "|."
}

vAaccE = \relative c' {
  \key aes \major
  \time 3/4
  aes4 ( bes ) c |
  des ( ees  f ) |
  g ( f ) ees |
  des (c bes ) |
  c (des) ees |
  f ( ees des ) |
  c (des) c |
  bes (aes g) |
  aes (bes) c |
  des (ees) f |
  g aes g |
  f (ees des) |
  ees (des) c |
  bes c des |
  c des ees |
  f (g aes) |
  g (f) ees |
  des c bes |
  aes (bes c |
  des ees f) |
  g (aes) g |
  f ees des |
  c (des c |
  bes aes) r  \bar "|."
}

vAaccF = \relative c'' {
  \key aes \major
  \time 3/4
  bes2 bes4 |
  g2 r4 |
  bes2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  f2 f4 |
  des2 r4 |
  des2 des4 |
  bes2 bes4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  f2 f4 |
  des2 des4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 \bar "|."
}

vAaccG = \relative c'' {
  \key aes \major
  \time 3/4
  aes4 ( ees ) c' |
  g (ees c' ) |
  aes4 ( ees ) c' |
  g (ees c' ) |
  f, (des) bes |
  bes' ( g ees ) |
  c' (aes ) ees |
  aes (ees) c' |
  aes (f) des |
  des' aes des |
  \bar "|."
}

melodyB = \relative c' {
  \key aes \major
  \time 3/4
  ees4. ( f8 ) ees4 |
  c2 r4 |
  ees4. ( f8 ) ees4 |
  c2 r4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  f2 f4 |
  aes4. ( g8 ) f4 |
  ees4. f8 ees4 |
  c2 r4 |
  f2 f4 |
  aes4. ( g8 ) f4 |
  ees4. f8 ees4 |
  c2 r4 |
  bes'2 bes4 |
  des4. bes8 g4 |
  aes2. ( |
  c2 ) r4 |
  aes4 ( ees4 ) c4 |
  ees4. des8 bes4 |
  aes2. ~ |
  aes2 r4 \bar "|."
}

melodyC = \relative c' {
  \key aes \major
  \time 3/4
  ees4. ( f8 ) ees4 |
  c2 r4 |
  ees4. ( f8 ) ees4 |
  c2 r4 |
  bes'2 bes4 |
  g2 r4 |
  aes2 aes4 |
  ees2 r4 |
  f2 f4 |
  aes4. ( g8 ) f4 |
  ees4. f8 ees4 |
  c2 r4 |
  f2 f4 |
  aes4. g8 f4 |
  ees4. ( f8 ) ees4 |
  c2 r4 |
  bes'2 bes4 |
  des4. bes8 g4 |
  aes2. ( |
  c2 ) r4 |
  aes4 ( ees4 ) c4 |
  ees4. des8 bes4 |
  aes2. ~ |
  aes2 r4 \bar "|."
}

vBaccAA = \relative c'' {
  \key aes \major
  \time 3/4
  aes8 ( bes c g ) bes ( aes ) |
  g ( f ees4 ) r4 |
  aes8 ( bes c g ) bes ( aes ) |
  g ( f ees4 ) r4 |
  bes'8 (c des g, ) bes ( aes ) |
  g ( f ees4 ) r |
  aes8 ( bes c g ) bes ( aes ) |
  g ( f ees4 ) r4 |
  aes8 ( bes c g ) bes ( aes ) |
  g ( f ) ees4 r4 |
  aes8 ( bes c ) g bes ( aes ) |
  g ( f ees4 ) r4 |
  bes'8 ( c des g, ) bes ( aes ) |
  g (f) ees4 r4 |
  aes8 ( bes c ) g bes ( aes ) |
  g ( f ees4 ) r4 |
  bes'8 ( c des g, ) bes ( aes ) |
  g f ees4 r4 |
  aes8 ( bes c g bes aes |
  g f ees4 ) r4 |
  aes8 ( bes c g ) bes ( aes ) |
  g ( f ) ees ( des' ) bes ( g ) |
  aes8 ( bes c g bes aes |
  g f ees4 ) r4 |
}
vBaccAB = \relative c' {
  \key aes \major
  \time 3/4
  R2.*3 |
  r2 c4 |
  des2 r4 
  bes2 bes4 |
  c2 r4 |
  r2 c4 |
  des2 r4 |
  R2. |
  r2 c4 |
  aes bes c |
  ees ( des2 ) |
  R2. |
  r2 c4 ( |
  aes ) bes c |
  ees2 r4 |
  ees4. ees8 des4 |
  c2. (|
  ees2.) |
  ees2. |
  r4 bes ( g) |
  aes2. ( |
  c2 ) r4 |
}

vBaccBA = \relative c'' {
  \key aes \major
  \time 3/4
  r8 aes8 ( g f ) ees ( des ) |
  c ( bes aes aes' g f ) |
  ees8 ( des c bes ) aes ( aes' ) |
  g ( f ees des c bes ) |
  aes8 (aes' g f ) ees ( des ) |
  c ( bes aes aes' g f ) |
  ees8 ( des c bes ) aes ( aes' ) |
  g ( f ees des c bes ) |
  aes8 (aes' g f ) ees ( des ) |
  c ( bes aes aes' ) g ( f ) |
  ees8 ( des c ) bes aes ( aes' ) |
  g ( f ees des c bes ) |
  aes8 (aes' g f ) ees ( des ) |
  c ( bes aes aes') g ( f ) |
  ees8 ( des c ) bes aes ( aes' ) |
  g ( f ees des c bes ) |
  aes8 (aes' g f ) ees ( des ) |
  c ( bes aes ) aes' g ( f ) |
  ees8 ( des c bes aes aes' |
  g f ees des c bes ) |
  aes8 (aes' g f ) ees ( des ) |
  c ( bes aes ) aes' g ( f ) |
  ees8 ( des c bes aes aes' |
  g f ees4 ) r |
}

vBaccBB = \relative c'' {
  \key aes \major
  \time 3/4
  R2. |
  r8 aes ( g f ees ) r |
  R2. |
  r2 aes8 (g |
  f ees) r4 r4 |
  R2. |
  r4. aes8 (g) f |
  ees aes (g) f ees4 |
  des4. r8 bes'4 ~ |
  bes2. ~ |
  bes4. ( aes8 ) g ( f ) |
  ees4 r4 bes' ~ |
  bes2. ~ |
  bes2 aes4 ~ |
  aes4 r8 aes g f |
  ees4 r f ~ |
  f2 g4 ~ |
  g2 r4 |
  r2 g4 ~ |
  g2 f4 ~ ( |
  f ees2) |
  r4 bes'4 g4 ~ |
  g f g |
  aes2 r4 |
}

vCaccAA = \relative c' {
  \key aes \major
  \time 3/4
  c2 c4 |
  aes2 r4 |
  c2 c4 |
  aes2 r4 |
  g'2 g4 |
  ees2 r4 |
  ees2 ees4 |
  c2 r4 |
  des2 des4 |
  f4. ( ees8 ) des4 |
  c4. des8 c4 |
  aes2 r4 |
  des2 des4 |
  f4. ( ees8 ) des4 |
  c4. des8 c4 |
  aes2 r4 |
  ees'2 ees4 |
  g4. aes8 ees4 |
  ees2. ( |
  aes2 ) r4 |
  ees2 c4 |
  des4. bes8 bes4 |
  aes2. ~ |
  aes2 r4 \bar "|."
}

vCaccAB = \relative c' {
  \key aes \major
  \time 3/4
  aes2 aes4 |
  aes2 r4 |
  aes2 aes4 |
  aes2 r4 |
  ees'2 ees4 |
  bes2 r4 |
  c2 c4 |
  aes2 r4 |
  aes2 aes4 |
  des2 aes4 |
  aes4. aes8 aes4 |
  aes2 r4 |
  aes2 aes4 |
  des2 aes4 |
  aes4. aes8 g4 |
  f2 r4 |
  g2 g4 |
  bes4. c8 des4 |
  c2. ( |
  ees2 ) r4 |
  c2 aes4 |
  bes4. aes8 g4 |
  aes2. ~ |
  aes2 r4 \bar "|."
}

vCaccBA = \relative c'' {
  \key aes \major
  \time 3/4
  c2 c4 |
  aes2 r4 |
  c2 c4 |
  aes2 r4 |
  des2 des4 |
  bes2 r4 |
  c2 c4 |
  aes2 r4 |
  des2 des4 |
  f4. (ees8) des4 |
  c4. des8 c4 |
  aes2 r4 |
  des2 des4 |
  f4. (ees8) des4 |
  c4. des8 c4 |
  aes2 r4 |
  ees'2 ees4 |
  ees4. ees8 des4 |
  c2. ( |
  ees2 ) r4 |
  c2 aes4 |
  bes4. bes8 g4 |
  aes2. ~ |
  aes2 r4 |
}

vCaccBB = \relative c'' {
  \key aes \major
  \time 3/4
  aes2 aes4 |
  ees2 r4 |
  aes2 aes4 |
  ees2 r4 |
  ees2 ees4 |
  ees2 r4 |
  ees2 ees4 |
  c2 r4 |
  des2 des4 |
  des2 des4 |
  aes'4. aes8 aes4 |
  ees2 r4 |
  des2 des4 |
  des2 des4 |
  aes'4. aes8 aes4 |
  ees2 r4 |
  g2 g4 |
  bes4. g8 ees4 |
  ees2. ~ |
  ees2 r4 |
  ees4 (c) ees |
  bes4. bes8 ees4 |
  c2. ~ |
  c2 r4 |
}

vCaccBC = \relative c' {
  \key aes \major
  \time 3/4
  c2 c4 |
  aes2 r4 |
  c2 c4 |
  aes2 r4 |
  g2 g4 |
  bes2 r4 |
  aes2 aes4 |
  aes2 r4 |
  aes2 aes4 |
  aes2 aes4 |
  aes4. aes8 aes4 |
  aes2 r4 |
  aes2 aes4 |
  aes2 aes4 |
  aes4. aes8 aes4 |
  aes2 r4 |
  ees'2 ees4 |
  ees4. ees8 bes4 |
  c2. ( |
  aes2 ) r4 |
  aes2 aes4 |
  g4. g8 ees4 |
  aes2. ~ |
  aes2 r4 |
}


vCaccCA = \relative c'' {
  \key aes \major
  \time 3/4
  R2. |
  c4. (des8) c4 |
  aes2 r4 |
  c4. (des8) c4 |
  des2 r4 |
  des2 des4 |
  c2 r4 |
  c2 c4 |
  aes2 r4 |
  des2 bes4 |
  c2 c4 |
  c4. des8 c4 |
  aes2 r4 |
  des2 bes4 |
  c2 c4 |
  c4. des8 c4 |
  bes2 r4 |
  bes2 bes4 |
  c4. des8 c4 |
  aes2. ( |
  c2 ) r4 |
  bes4 (g) ees |
  f4. g8 f4 |
  ees2 r4 |
}

vCaccCB = \relative c'' {
  \key aes \major
  \time 3/4
  R2. |
  aes2 aes4 |
  ees2 r4 |
  aes2 aes4 |
  ees2 r4 |
  bes'2 bes4 |
  aes2 r4 |
  aes2 aes4 |
  aes2 r4 |
  bes2 g4 |
  aes2 aes4 |
  aes4. bes8 aes4 |
  aes2 r4 |
  bes2 g4 |
  aes2 aes4 |
  aes4. bes8 aes4 |
  g2 r4 |
  g2 g4 |
  aes4. bes8 aes4 |
  f2. ( |
  ees2 ) r4 |
  g4 (ees) c |
  des4. ees8 des4 |
  c2 r4 |
}

vCaccCC = \relative c' {
  \key aes \major
  \time 3/4
  c4. (des8) c4 |
  aes2 r4 |
  c4. (des8) c4 |
  aes2 r4 |
  g'2 g4 |
  ees2 r4 |
  ees2 ees4 |
  c2 r4 |
  des2 des4 |
  f4. (ees8) des4 |
  c4. (des8) c4 |
  aes2 r4 |
  des2 des4 |
  f4. ees8 des4 |
  c4. des8 c4 |
  aes2 r4 |
  ees'2 ees4 |
  e4. e8 e4 |
  f2 ( ees4 |
  d2 ) r4 |
  ees4 (c) aes |
  des4. bes8 g4 |
  bes2. ~ |
  bes2 r4 |
}

vCaccDA = \relative c'' {
  \key aes \major
  \time 3/4
  bes2 bes4 |
  g2 r4 |
  bes2 bes4 |
  g2 r4 |
  des'2 des4 |
  bes2 r4 |
  c2 c4 |
  bes4 (aes) r4 |
  aes2 aes4 |
  bes2 aes4 |
  aes4. aes8 aes4 |
  aes2 r4 |
  aes2 aes4 |
  f4. f8 f4 |
  d4 (c) c |
  ees2 r4 |
  g2 g4 |
  bes4. g8 e4 |
  g4 ( f ees |
  aes g f ) |
  ees4 (c'4) aes |
  bes4. bes8 g4 |
  g2. ~ |
  g2 r4 |
}

vCaccDB = \relative c' {
  \key aes \major
  \time 3/4
   c4. (des8) c4 |
   ees2 r4 |
   c4. (des8) c4 |
   ees2 r4 |
   ees2 ees4 |
   e2 r4 |
   g (f) ees |
   ges2 r4 |
   des2 des4 |
   f4. (ees8) des4 |
   c4. des8 c4 |
   bes4 (aes) r |
   des2 des4 |
   d4. d8 d4 |
   bes2 bes4 |
   a2 r4 |
   f'2 f4 |
   g4. e8 c4 |
   ees2. ( |
   f2 ) r4 |
   c4 (aes) aes |
   g4 g' ees |
   ees2. ~ |
   ees2 r4 |
}

vCaccDC = \relative c' {
  \key aes \major
  \time 3/4
  aes2 aes4 |
  aes2 r4 |
  aes2 g4 |
  f2 r4 |
  g2 bes4 |
  des2 r4 |
  c2 c4 |
  c2 r4 |
  c4 (bes) aes |
  des2 bes4 |
  aes4 aes g |
  f2 r4 |
  c'4 (bes) bes |
  b4. b8 b4 |
  g2 g4 |
  fis2 r4 |
  des'2 des4 |
  e4. c8 bes4 |
  c2. ( |
  des2 ) r4 |
  ees,2 ees4 |
  ees4. ees8 ees4 |
  f2 (g4 |
  c2 ) r4 \bar "|."
}


vCaccEA = \relative c' {
  \key aes \major
  \time 3/4
  <c d>2 <c d>4 |
  <aes bes>2 r4 |
  <c d>2 <c d>4 |
  <aes bes>2 r4 |
  <ees' g>2 <ees g>4 |
  <c d>2 r4 |
  <des f>2 <des f>4 |
  <bes c>2 r4 |
  <bes des>2 <bes des>4 |
  <des fes>4. ( <bes ees>8 ) <bes ees>4 |
  <aes c>4. <aes des>8 <aes c>4 |
  <g aes>2 r4 |
  <c des>2 <c des>4 |
  <des f>4. <des ees>8 <bes des>4 |
  <aes c>2 <aes c>4 |
  <aes bes>2 r4 |
  <ees' ges>2 <ees f>4 |
  <ees g>4. <des e>8 <des ees>4 |
  <des f>2. ( | 
  <ees ges>2 ) r4 |
  <ees f>4 (<bes c>) <g aes> |
  <bes des>4. <bes des>8 <g bes>4 |
  f2. ~ |
  f2 r4 |
}

vCaccEB = \relative c' {
  \key aes \major
  \time 3/4
  aes2 aes4 |
  g2 r4 |
  aes2 aes4 |
  g2 r4 |
  aes2 aes4 |
  aes2 r4 |
  bes2 bes4 |
  ges2 r4 |
  aes2 aes4 |
  beses4. ( aes8 ) g4 |
  f4. f8 f4 |
  f2 r4 |
  aes2 aes4 |
  bes4. bes8 aes4 |
  f4. (g8) f4 |
  ees2. |
  ces'2 ces4 |
  bes4. g8 aes4 |
  bes2. ( |
  bes2 ) r4 |
  c4 (g) ees |
  g4. g8 ees4 |
  ees2. ~ |
  ees2 r4 |
}

vCaccEC = \relative c {
  \key aes \major
  \time 3/4
  \clef bass
  bes2 bes4 |
  ees2 r4 |
  bes2 bes4 |
  ees2 r4 |
  f,2 f4 |
  bes2 r4 |
  ees2 ees4 |
  aes,2 r4 |
  ges2 ges4 |
  ces4. (ges8) des'4 |
  bes4. bes8 bes4 |
  ees2 r4 |
  bes2 bes4 |
  ees4. ees8 aes,4 |
  des2 des4 |
  ges,2 r4 |
  des'2 des4 |
  c4. c8 bes4 |
  ees2. ( |
  aes,2 ) r4 |
  des2 bes4 |
  ees4. ees8 aes,4 |
  des2. ( |
  aes2 ) r4 |
}

vCaccFA = \relative c'' {
  \key aes \major
  \time 3/4
  bes4. g4. |
  f4. (g4.) |
  bes4. g4. |
  f4. (g4.) |
  g f |
  ees (f) |
  g f |
  g (aes) |
  bes aes |
  f aes |
  bes g |
  f g |
  bes aes |
  f aes |
  bes g |
  f g |
  c aes |
  g f |
  c' (aes) |
  g (f) |
  c' aes |
  g f |
  c' (aes) |
  g (f) |

}

vCaccFB = \relative c' {
  \key aes \major
  \time 3/4
  f2 des4 ~ |
  des ees2 |
  f2 des4 ~ |
  des ees2 |
  ees2 ees4 ~ |
  ees ees2 |
  ees2 ees4 ~ |
  ees ees2 |
  des2 des4 ~ |
  des des2 |
  aes2 bes4 ~ |
  bes c2 |
  des2 des4 ~ |
  des des2 |
  aes2 bes4 ~ |
  bes c2 |
  ees2 ees4 ~ |
  ees ees2 |
  ees2 ees4 ~ |
  ees des2 |
  ees2 ees4 ~ |
  ees ees2 |
  ees2 ees4 ~ |
  ees ees2 |
}

vCaccFC = \relative c' {
  \key aes \major
  \time 3/4
  aes2 ~ aes8 bes ~ |
  bes2 c4 ~ |
  c4. aes4. ~ |
  aes4 bes2 ~ |
  bes8 c ~ c2 |
  %
  des2 ~ des8 c ~ |
  c2 bes4 ~ |
  bes4. aes4. ~ |
  aes4 bes2 ~ |
  bes8 c ~ c2 |
  %
  des2 ~ des8 ees ~ |
  ees2 aes,4 ~ |
  aes4. bes4. ~ |
  bes4 c2 ~ |
  c8 aes ~ aes2 |
  %
  bes2 ~ bes8 c ~ |
  c2 bes4 ~ |
  bes4. des4. ~ |
  des4 c2 ~ |
  c8 bes ~ bes2 |
  c2 ~ c8 bes8 ~ |
  bes2 c4 ~ |
  c4. aes4. ~ |
  aes4 r r |
}

vCaccHA = \relative c'' {
  \key aes \major
  \time 3/4
  r2 r8 bes ~ |
  bes4. g |
  f4 (g4.) r8 |
  bes4. g |
  f'2. |
  des2. |
  g4. f4. |
  f4. (ees4.) |
  ees4 (des2)  |
  r4 d2 | %%%%%% stream
  f4 d2 |
  d4. c  |
  ees2. |
  r4 d2 |
  f4 d2 |
  d4. (ees) |
  f2. ( |
  g2. ~ |
  g4 ) r4 f4 ~ |
  f2 ees4~ |
  ees2. ~ |
  ees4 r8 
  bes4. | g f |
  ees2. \bar "|."
}

vCaccHB = \relative c'' {
  \key aes \major
  \time 3/4
  r2 r8 g ~ |
  g4. ees |
  des4 (ees4.) r8 |
  g4. ees |
  des'2. |
  c2. |
  ees4. des |
  des4. (c) |
  c2. |
  r4 c2 |
  d4 bes2 |
  bes4. aes |
  des2. |
  r4 c2 |
  c4 c2 |
  bes4. c |
  ees2. ~ |
  ees2. ~  |
  ees4 r4 ees4 ~ |
  ees2 c4 ~( |
  c2. |
  des4) r8 
  g,4. | ees des |
  c2. \bar "|."
}
vCaccHC = \relative c' {
  \key aes \major
  \time 3/4
  r2 r8 ees ~ |
  ees4. c |
  bes4 (c4.) r8 |
  ees4. c |
  c'2. |
  bes2. |
  c4. c |
  bes4. (a4.) |
  bes2. |
  r4 bes2 |
  bes4 g2 |
  g4. g |
  bes2. |
  r4 bes2 |
  bes4 bes2 |
  aes4. (a) |
  des2. ( |
  f ~ |
  f4) r4 des4 ~ |
  des2 aes4 ~ (|
  aes2. |
  bes4) r8
  ees,4. | des c |
  bes2. \bar "|."
}
vCaccHD = \relative c' {
  \key aes \major
  \time 3/4
  r2 r8 c ~ |
  c4. bes |
  aes4 (bes4.) r8 |
  c4. bes |
  aes'4. (ees) |
  f4. (ees) |
  bes'4. bes |
  ges2. |
  aes2. |
  r4 f2 |
  g4 f2 |
  ees4. f |
  aes2. |
  r4 f2 |
  g4 g2 |
  f4. (ges) |
  c2. ~ |
  c2. ~  |
  c4  r4 g4 ~ |
  g2 f4 ~ ( |
  f2. |
  g4) r8 
  c,4. | c bes |
  aes2. \bar "|."
}

lMelodyA = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  All is calm, all is bright.
  Round yon Vir -- gin, Mo -- ther and Child.
  Ho -- ly In -- fant so ten -- der and mild.
  Sleep in hea -- ven -- ly peace.
  Sleep in hea -- ven -- ly peace.
}
lvAaccF = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  All is calm, all is bright.
  Round yon Vir -- gin, All is calm, all is bright.
  All is calm, all is bright.
  Round yon Vir -- gin, All is calm, all is bright.
}
lMelodyB = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  Shep -- herds quake at the sight.
  Glo -- ries stream from hea -- ven a -- far.
  Heav -- 'nly hosts sing Hal -- le -- lu -- ia!
  Christ the Sa -- vior is born.
  Christ the Sa -- vior is born.
}
lvBaccAB = \lyricmode {
  ly night
  Shep -- herds quake
  the sight.
  A -- far Glo -- ries stream
  Hal -- le -- lu -- ia!
  Sa -- vior is born.
  Christ is born.
}
lvBaccBB = \lyricmode {
  a
  a
  At the sight
  At the sight
  Glo
  Glo -- ries stream
  far sing
  Hal -- le -- lu -- ia!
  Christ is born
  Christ is born
  Christ is born
}
lMelodyC = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  Son of God, love's pure light.
  Ra -- diant beams from Thy ho -- ly face.
  With the dawn of re -- deem -- ing grace.
  Je -- sus Lord, at Thy birth.
  Je -- sus Lord, at Thy birth.
}
lvCaccFB = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  Son of God, love's pure light.
  Ra -- diant beams Thy ho -- ly.
  With dawn of re -- deem -- ing.
  Je -- sus Lord, at Thy birth.
  Je -- sus Lord, at Thy birth.
}
lvCaccFC = \lyricmode {
  Si -- lent night,
  ho -- ly night!
  Son of love's light.
  Ra -- diant from ho -- ly
  dawn, re -- deem -- ing grace.
  Je -- sus Lord birth.
  Je -- sus Lord birth.
}

lvCaccHA = \lyricmode {
  Si -- lent night,
  ho -- ly
  Shep -- herds at the sight.
  Glo
  stream
  Glo -- ries stream a -- far
  hosts
  Hal -- le -- lu -- ia!
  is born.
  Sa -- vior is born.
}

\include "longs.ly"

\markup { \bold "Sostenuto" }
\noPageBreak
\new ChoirStaff <<
  \new Staff \with { \override TimeSignature.stencil = ##f } { \new Voice = "melody" \longs }
  \new Lyrics \lyricsto "melody" { \longsL }
>>


\markup { \bold "Verse 1" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyA } }
  \new Lyrics \lyricsto "melody" { \lMelodyA }  
>>


\markup { \bold "Verse 2, Take 1" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB }  
  \new Staff { \new Voice = "chimes" \relative c'' {  \key aes \major
  \time 3/4
 r4 aes2 ~
  aes4 ees2 |  r4 aes2 ~ | aes4 ees2 | r4 des'2 ~ | des4 bes2 | r4 c2 ~ | c4 aes2 |
  r4 aes2 ~ | aes4 bes2 ~ | bes4 c2 ~ | c4 aes2 | r4 aes2 ~ | aes4 bes2 ~ | bes4 g2 ~ | g4 aes2 ~ | aes4 g f | g ees2 | r4 c2 ( ~ | c4 ees2) | r4 c2 | r4 g'2 | r4 c2 ( | aes2) r4 |

   } }
  \new Lyrics \lyricsto "chimes"  {
    Si -- lent ho -- ly Shep -- herds at the
    Glo -- ries hea -- ven
    Hal -- le -- lu -- ia Christ the Sa -- vior
    Born. Christ is Born.
  }  

>>

\markup { \bold "Verse 2, Take 2" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB } 
  \new Staff { \new Voice = "harmA" { \vBaccA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
>>

\markup { \bold "Verse 2, Take 2" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB } 
  \new Staff { \new Voice = "harmA" { \vBaccCpt } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
>>

\markup { \bold "Verse 2, Take 4" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB } 
  \new Staff { \new Voice = "harmA" { \vBaccSimp } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
>>

\markup { \bold "Verse 2, Take 5" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "harmA" { \vBaccAA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB }  
  \new Staff { \new Voice = "harmB" { \vBaccAB } }
  \new Lyrics \lyricsto "harmB" { \lvBaccAB }  
>>


\markup { \bold "Verse 2, Take 6" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "harmA" { \vCaccCA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
  \new Staff { \new Voice = "harmB" { \vCaccCB } }
  \new Lyrics \lyricsto "harmB" { \lMelodyB }  
  \new Staff { \new Voice = "melody" { \melodyC } }
  \new Lyrics \lyricsto "melody" { \lMelodyB }
  \new Staff { \new Voice = "harmC" { \vCaccCC } }
  \new Lyrics \lyricsto "harmC" { \lMelodyB }  
>>

\markup { \bold "Verse 2, Take 7" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "harmA" { \vBaccBA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyB }  
  \new Staff { \new Voice = "harmB" { \vBaccBB } }
  \new Lyrics \lyricsto "harmB" { \lvBaccBB }  
  \new Staff { \new Voice = "melody" { \melodyB } }
  \new Lyrics \lyricsto "melody" { \lMelodyB }  
>>

\markup { \bold "Verse 2, Take 8" }
\noPageBreak
\new Score <<
  \new Staff \with { \magnifyStaff #2/3 } { \new Voice = "melody" { \melodyB } }
  \new Lyrics  \lyricsto "melody" { \set fontSize = #-2 \lMelodyB }
  \new ChoirStaff <<
    \new Staff { \new Voice = "harmA" { \vCaccHA } }
    \new Lyrics \lyricsto "harmA" { \lvCaccHA }  
    \new Staff { \new Voice = "harmB" { \vCaccHB } }
    \new Lyrics \lyricsto "harmB" { \lvCaccHA }
    \new Staff { \new Voice = "harmC" { \vCaccHC } }
    \new Lyrics \lyricsto "harmC" { \lvCaccHA }  
    \new Staff { \new Voice = "harmD" { \vCaccHD } }
    \new Lyrics \lyricsto "harmD" { \lvCaccHA }  
  >>
>>

\markup { \bold "Verse 3" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyC } }
  \new Lyrics \lyricsto "melody" { \lMelodyC }  
>>

\markup { \bold "Verse 3 :: Alt 1" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyC } }
  \new Lyrics \lyricsto "melody" { \lMelodyC }  
  \new Staff { \new Voice = "harmA" { \vCaccAA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyC }  
  \new Staff { \new Voice = "harmB" { \vCaccAB } }
  \new Lyrics \lyricsto "harmB" { \lMelodyC }  
>>

\markup { \bold "Verse 3 :: Alt 2" }
\noPageBreak
\new ChoirStaff <<
  \new Staff { \new Voice = "melody" { \melodyC } }
  \new Lyrics \lyricsto "melody" { \lMelodyC }
  \new Staff { \new Voice = "harmA" { \vCaccEA } }
  \new Lyrics \lyricsto "harmA" { \lMelodyC }  
  \new Staff { \new Voice = "harmB" { \vCaccEB } }
  \new Lyrics \lyricsto "harmB" { \lMelodyC }  
  \new Staff { \new Voice = "harmC" { \vCaccEC } }
  \new Lyrics \lyricsto "harmC" { \lMelodyC }  
>>




% one could start with <c ees f a>   --- then <a c d fis> ?