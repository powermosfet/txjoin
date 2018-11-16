# txjoin

hledger utility that takes transaction imported from multiple bank accounts (typically from CSV) and merges matching transactions

## Example

txjoin turns this:

```
2018/09/13 a
    assets:bank:brk    200
    income:pay

2018/09/14 a
    assets:bank:brk    -50
    expenses:unknown

2018/09/14 b
    assets:bank:brk    -50
    expenses:unknown

2018/09/14 a
    assets:bank:rgn      50
    income:unknown
```

...into this:

```
2018/09/13 a
    assets:bank:brk    200
    income:pay

2018/09/14 a
    assets:bank:brk    -50
    assets:bank:rgn

2018/09/14 b
    assets:bank:brk    -50
    expenses:unknown
```
