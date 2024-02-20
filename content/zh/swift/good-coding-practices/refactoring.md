---
date: 2024-01-26 03:36:39.058429-07:00
description: "\u91CD\u6784\u662F\u91CD\u7EC4\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\
  \u7684\u8FC7\u7A0B\uFF0C\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\
  \u5E93\uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\u3001\u53EF\u7EF4\u62A4\u6027\uFF0C\u5E76\
  \u4E3A\u5C06\u6765\u7684\u7279\u6027\u94FA\u8DEF\uFF0C\u540C\u65F6\u5C3D\u91CF\u51CF\
  \u5C11\u6280\u672F\u503A\u52A1\u3002"
lastmod: 2024-02-19 22:05:07.226987
model: gpt-4-0125-preview
summary: "\u91CD\u6784\u662F\u91CD\u7EC4\u73B0\u6709\u8BA1\u7B97\u673A\u4EE3\u7801\
  \u7684\u8FC7\u7A0B\uFF0C\u800C\u4E0D\u6539\u53D8\u5176\u5916\u90E8\u884C\u4E3A\u3002\
  \u7A0B\u5E8F\u5458\u8FDB\u884C\u91CD\u6784\u662F\u4E3A\u4E86\u6E05\u7406\u4EE3\u7801\
  \u5E93\uFF0C\u63D0\u9AD8\u53EF\u8BFB\u6027\u3001\u53EF\u7EF4\u62A4\u6027\uFF0C\u5E76\
  \u4E3A\u5C06\u6765\u7684\u7279\u6027\u94FA\u8DEF\uFF0C\u540C\u65F6\u5C3D\u91CF\u51CF\
  \u5C11\u6280\u672F\u503A\u52A1\u3002"
title: "\u91CD\u6784"
---

{{< edit_this_page >}}

## 什么以及为什么？
重构是重组现有计算机代码的过程，而不改变其外部行为。程序员进行重构是为了清理代码库，提高可读性、可维护性，并为将来的特性铺路，同时尽量减少技术债务。

## 如何进行：
我们从一个基本的Swift示例开始，其中有一些重复的代码：

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("名字: \(firstName)")
    print("姓氏: \(lastName)")
    print("年龄: \(age)")
}

func printUserJob(title: String, company: String) {
    print("职位: \(title)")
    print("公司: \(company)")
}
```

重构包括创建一个 `User` 结构体来封装用户属性，并添加一个打印详细信息的方法：

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("名字: \(firstName)")
        print("姓氏: \(lastName)")
        print("年龄: \(age)")
        print("职位: \(jobTitle)")
        print("公司: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "软件开发者", company: "技术解决方案")
user.printDetails()
```

### 样本输出：
```
名字: John
姓氏: Doe
年龄: 30
职位: 软件开发者
公司: 技术解决方案
```

## 深入探讨
重构的根源可以追溯到软件工程的早期，但这个术语是在1990年代末通过Martin Fowler的开创性书籍《重构：改善既有代码的设计》而普及的。该书阐述了代码应该继续通过小步骤清理，而不是等待单独的阶段这一原则。

手动重构的替代方案包括自动化工具和IDE(集成开发环境)，它们可以帮助检测重复代码、建议简化以及自动生成代码的部分。对于Swift开发，Xcode提供了各种重构工具，如重命名和提取方法功能，这些工具可以减少过程中的人为错误潜在性。

实施重构时，重要的是要有一个稳固的测试套件。测试充当一个安全网，确保你所做的改变不会引入错误。这是至关重要的，因为重构的主要目标是在不影响外部行为的情况下更改内部结构。

## 另见
- [《重构：改善既有代码的设计》by Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [苹果的Swift文档](https://swift.org/documentation/)
- [使用Xcode重构工具](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray Wenderlich的Swift风格指南](https://github.com/raywenderlich/swift-style-guide)
