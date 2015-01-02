# denotational-frp

> Denotational semantics for discrete-time FRP

## Example's output

```
concatMap . flatMap . zip
s1    = a.....b..............c
s2    = ..1.2.3
zip   = ..a1..b2.............c3
      = ......a...1...b...2......c...3
concatMap . merge
s1    = a.....b..............c
s2    = ..1.2.3
merge = a.1.2.b..............c
      = ....a...1...2...b...3....c
fairScheduler
s1    = a.....b..............c
s2    = ..1.2.3
fair  = ....a...1...b...2...3....c
```