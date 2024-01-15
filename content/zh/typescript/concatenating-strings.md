---
title:                "连接字符串"
html_title:           "TypeScript: 连接字符串"
simple_title:         "连接字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
在编程中，有时候我们需要将多个字符串拼接在一起来生成新的字符串。这样可以帮助我们更有效地处理文本数据，比如拼接用户名和网站地址来生成用户的个人主页链接。 

## 如何使用
TypeScript中，我们可以使用"+"运算符来连接两个字符串，示例如下：
```TypeScript
let name:string = "小明";
let website:string = "www.example.com";
let link:string = name + "的个人主页是：" + website;

console.log(link);
```
输出结果为："小明的个人主页是：www.example.com"

## 深入了解
除了使用"+"运算符，TypeScript也提供了字符串模板的方式来拼接字符串。使用反引号(`)包裹需要拼接的字符串，然后在需要插入变量的位置使用${}包裹变量名称。示例如下：
```TypeScript
let name:string = "小明";
let website:string = "www.example.com";
let link:string = `${name}的个人主页是：${website}`;

console.log(link);
```
输出结果同样为："小明的个人主页是：www.example.com"。这种方式更加简洁易读，在拼接较长的字符串时也更容易排版和维护。

## 参考资料
- [TypeScript官方文档：字符串](https://www.typescriptlang.org/docs/handbook/strings.html)
- [Understanding String concatenation in TypeScript](https://medium.com/@vvkchandra/understanding-string-concatenation-in-typescript-c9d21a2f293d)
- [TypeScript字符串模板的使用](https://www.cnblogs.com/leon1984/p/5091030.html)

## 参见
- [TypeScript中文网](https://www.tslang.cn/docs/handbook/basic-types.html)
- [如何快速上手TypeScript](https://juejin.im/post/5cdaf739f265da0373710e6e)