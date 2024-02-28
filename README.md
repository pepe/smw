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