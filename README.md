# fpproject

## 语法

parser忽略所有空白符。注意：我们不会因为大小写而区分类型还是变量。parser的语法如下所示。

```
expr :=
    | (expr)
    | True | False                      // Bool常量
    | 0 | 1 | -1 | ...                  // 整型常量包括非负数和负数
    | 'a' | 'b' ...                     // 字符常量支持转义
    | if expr1 then expr2 else expr3    // if语句
    | \id :: type => expr               // lambda表达式
    | let id = expr1 in expr2           // let表达式
    | letrec id1 = \id2 :: type1
        => expr1 :: type2 in expr2      // 递归的let表达式
    | case expr of
        pattern1 => expr1;
        pattern2 => expr2;
        ...                             // case语句，结尾一定有分号，至少有一个模式匹配
    | expr1 operator expr2              // 双目运算符，将在之后给出定义

id := a | a0 | ...                      // 字母开头后可跟字母或数字

type :=
    | Bool
    | Int
    | Char
    | id                                // 被认为是代数数据类型的名字
    | type1 -> type2                    // 函数类型，右结合

pattern :=
    | (pattern)
    | True | False                      // Bool常量模式
    | 0 | 1 | -1 | ...                  // 整型常量模式包括非负数和负数
    | id pattern1 pattern2 ...          // 代数数据类型模式，至少有一个子模式
    | id                                // 变量模式

adt :=
    data id = id1 type11 type12 ... | id2 type21 type22 ... | ...
                                        // 注意：这里的竖线是字符的意思。结尾不需要竖线。
                                        // 至少有一个数据构造函数。每个数据构造函数可有零个或多个类型。

statement := expr | adt
```

expr的运算符如下。注意：“$”运算符拥有最高优先级，且是左结合的。

| operator | 优先级 | 结合性 |
|:--------:|:-----:|:-----:|
| !        | 8     |       |
| $        | 7     | 左    |
| *        | 6     | 左    |
| /        | 6     | 左    |
| %        | 6     | 左    |
| +        | 5     | 左    |
| -        | 5     | 左    |
| <        | 4     | 左    |
| >        | 4     | 左    |
| <=       | 4     | 左    |
| >=       | 4     | 左    |
| ==       | 3     | 左    |
| !=       | 3     | 左    |
| &&       | 2     | 左    |
| ||       | 1     | 左    |

parser不断读入statement，遇到是expr的求值，遇到是adt的，则存入全局状态中。

## 测试

通过`stack test :base-test`可以跑助教的测试。类似地，通过`stack test :parser-test`可以跑我们的parser的测试
