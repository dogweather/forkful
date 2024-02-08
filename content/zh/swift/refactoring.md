---
title:                "重构"
aliases:
- zh/swift/refactoring.md
date:                  2024-01-26T03:36:39.058429-07:00
model:                 gpt-4-0125-preview
simple_title:         "重构"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/refactoring.md"
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
