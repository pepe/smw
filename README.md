# See My Work

Yet another presentation system writen in Janet and gp.

Run the application with:

```
> jpm -l janet .\app\init.janet
```

Run the restarting server with: 

```
jpm -l janet -l gp/utils -e '(watch `janet` `./app/init.janet`)'
```

## Example presentation

```
# Backend CULS 2024

---
author: Josef Pospíšil
date: 2024-02-26
title: The Intro
---

## Future Backend Development

---

## Good Old Friends

* Hello again!
* Black Swan Song

---
```