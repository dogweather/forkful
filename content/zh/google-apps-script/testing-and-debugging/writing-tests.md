---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:44.759057-07:00
description: "\u5728\u8C37\u6B4C\u5E94\u7528\u811A\u672C\uFF08Google Apps Script,\
  \ GAS\uFF09\u4E2D\u7F16\u5199\u6D4B\u8BD5\uFF0C\u662F\u5173\u4E8E\u521B\u5EFA\u81EA\
  \u52A8\u5316\u811A\u672C\u4EE5\u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u884C\u4E3A\uFF0C\
  \u786E\u4FDD\u5B83\u4EEC\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\u6309\u9884\u671F\u5DE5\
  \u4F5C\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\
  \u73B0\u9519\u8BEF\u3001\u63D0\u9AD8\u4EE3\u7801\u8D28\u91CF\uFF0C\u4EE5\u53CA\u4FBF\
  \u4E8E\u66F4\u65B0\u548C\u7EF4\u62A4\u3002"
lastmod: '2024-03-13T22:44:47.206500-06:00'
model: gpt-4-0125-preview
summary: "\u5728\u8C37\u6B4C\u5E94\u7528\u811A\u672C\uFF08Google Apps Script, GAS\uFF09\
  \u4E2D\u7F16\u5199\u6D4B\u8BD5\uFF0C\u662F\u5173\u4E8E\u521B\u5EFA\u81EA\u52A8\u5316\
  \u811A\u672C\u4EE5\u9A8C\u8BC1\u60A8\u7684\u4EE3\u7801\u884C\u4E3A\uFF0C\u786E\u4FDD\
  \u5B83\u4EEC\u5728\u5404\u79CD\u6761\u4EF6\u4E0B\u6309\u9884\u671F\u5DE5\u4F5C\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u5C3D\u65E9\u53D1\u73B0\u9519\
  \u8BEF\u3001\u63D0\u9AD8\u4EE3\u7801\u8D28\u91CF\uFF0C\u4EE5\u53CA\u4FBF\u4E8E\u66F4\
  \u65B0\u548C\u7EF4\u62A4\u3002"
title: "\u7F16\u5199\u6D4B\u8BD5"
weight: 36
---

## 什么及为什么？

在谷歌应用脚本（Google Apps Script, GAS）中编写测试，是关于创建自动化脚本以验证您的代码行为，确保它们在各种条件下按预期工作。程序员这样做是为了尽早发现错误、提高代码质量，以及便于更新和维护。

## 如何操作：

虽然谷歌应用脚本没有像其他一些编程环境那样内置测试框架，但您仍然可以通过利用简单的GAS函数或集成外部测试库（如`QUnit`）来编写和运行测试。这里有一个使用简单GAS函数测试脚本中另一个函数的基础示例：

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var result = add(2, 3);
  if (result !== 5) {
    throw new Error("测试失败：add(2, 3) 应该是 5, 但是是 " + result);
  } else {
    Logger.log("测试通过！");
  }
}
```

运行`testAdd()`如果`add`函数正确工作，会记录"测试通过！"，如果不正确，则抛出错误。对于更精细化的方法，将QUnit与谷歌应用脚本集成涉及一些更多步骤，但提供了一个强大的测试环境。一个QUnit测试设置的示例如下所示：

1. 在您的项目中包含QUnit库。
2. 为运行QUnit测试创建一个测试HTML文件。
3. 使用QUnit的语法编写测试用例。

这里是使用QUnit的一个例子：

```javascript
// 通过在用于运行测试的HTML文件中链接到它来包含QUnit

QUnit.test("测试add函数", function (assert) {
  var result = add(2, 3);
  assert.equal(result, 5, "add(2, 3) 应返回 5");
});
```

要查看结果，在GAS脚本编辑器中打开HTML文件，或将其作为Web应用部署。

## 深入探讨

历史上，在谷歌应用脚本中的测试有些被忽视，这可能是因为该平台的起源和主要用例专注于快速、小规模的自动化任务，而不是大型应用程序。因此，GAS没有提供在更传统的编程环境中找到的那些健壮的测试框架和工具。然而，社区通过创造性地利用开源库和谷歌现有的工具进行了适应。

使用像QUnit这样的库代表了向前迈出的一大步，但也带来了自己的一套挑战，例如设置合适的测试环境和学习额外的语法。然而，对于那些投资于使用GAS构建更复杂和可靠的应用程序的人来说，这种努力是值得的。

使用简单的GAS函数进行测试等替代方法提供了易用性和与GAS环境的集成，但缺乏全面的测试功能，且随着项目增长，难以轻松扩展。像clasp（谷歌应用脚本命令行界面）这样的工具可以促进更高级的工作流程，包括测试，通过允许开发者在其首选IDE中编码，为与外部测试框架更无缝集成提供了空间。

总之，虽然GAS可能没有开箱即用的复杂测试支持，但其灵活性和社区的创新方法提供了可行的路径，以确保您的脚本健壮、可靠，并且随时准备执行任何任务。
