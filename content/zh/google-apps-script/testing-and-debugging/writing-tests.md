---
title:                "编写测试"
aliases: - /zh/google-apps-script/writing-tests.md
date:                  2024-02-01T22:08:44.759057-07:00
model:                 gpt-4-0125-preview
simple_title:         "编写测试"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/google-apps-script/writing-tests.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
