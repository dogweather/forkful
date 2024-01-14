---
title:    "Elixir: 编写测试"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# 为什么写测试？

在编程世界中，编写测试是一个必不可少的步骤。它可以帮助我们发现可能存在的错误并提前解决它们。写测试可以增加代码的可靠性和稳定性，让我们在后续的开发过程中更加放心。同时，它也可以节省我们宝贵的时间，因为我们不必手动测试每一行代码。测试是一个不可或缺的程序员技能，让我们一起来学习如何编写测试吧！

## 如何编写测试

编写测试是一件非常简单的事情，只需按照以下步骤即可：

1. 首先，在你的项目文件夹下创建一个名为"test"的文件夹。
2. 在"test"文件夹中创建一个与你想要测试的模块同名的文件，比如"example_test.exs"。
3. 在文件中使用`ExUnit.Case`模块来定义一个测试模块，比如`defmodule ExampleTest do`。
4. 在`ExUnit.Case`模块中，使用`test`来定义你的测试用例，比如`test "addition" do`。
5. 写下你期望的结果，比如`assert 2 + 2 == 4`。
6. 在命令行中运行`mix test`来执行测试。如果所有测试通过，你会看到一个绿色的"1 test, 0 failures"的输出，否则会有相应的错误提示。

下面是一个简单的例子：

```Elixir
defmodule ExampleTest do
  use ExUnit.Case
  
  test "addition" do
    assert 2 + 2 == 4
  end
end
```

运行`mix test`得到的输出为：

```
==> example 
Compiling 1 file (.ex) 
. 
Finished in 0.07 seconds 
1 test, 0 failures 
Randomized with seed 409943
```

恭喜！你已经成功地编写了一个简单的测试用例。

## 深入了解

除了基本的`assert`函数外，Elixir还提供了其他一些方便的函数来帮助你编写更加全面和有效的测试用例。比如，你可以使用`refute`函数来检查一个表达式是否为假，或者使用`assert_receive`函数来测试进程间的消息传递。此外，你还可以使用`ExUnit`模块中的一些特殊注解来进行更高级的测试，比如`@tag`和`@moduletag`等。通过学习和了解这些功能，你可以编写出更加复杂和全面的测试用例。

在编写测试时，还有一些最佳实践可以帮助你写出更加清晰和可维护的代码。比如，避免重复代码，使用变量来减少冗余，以及编写可读性高的测试用例名称等等。通过学习这些最佳实践，你可以提高写测试的效率和质量。

现在你已经掌握了基础的测试知识，接下来就是不断练习和探索，让编写测试成为你的日常习惯吧！

# 参考资料

- [Elixir官方文档 - 单元测试](https://elixir-lang.org/getting-started/mix-otp/docs-tests-and-pipelines.html)
- [Elixir官方文档 - ExUnit库](https://hexdocs.pm/ex_unit/ExUnit.html)
- [Elixir School - 单元测试](https://elixirschool.com/blog/testing-with-exunit/)
- [Elixir Forum - 单元测试最佳实践](https://elixirforum.com/t/best-practices-for-exunit-testing/3674)

# 查看也可以

- [Elixir教程](https://elixirschool.com/zh