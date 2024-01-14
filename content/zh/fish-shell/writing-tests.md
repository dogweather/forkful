---
title:    "Fish Shell: 编写测试"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

# 为什么写测试

在编写代码的过程中，有时候会出现一些意想不到的bug。而测试则可以帮助我们发现并修复这些问题，保证代码的质量和稳定性。因此，编写测试是一项非常重要的技能，可以帮助我们提升代码的可靠性和可维护性。

## 如何编写Fish Shell测试

编写Fish Shell测试有几个重要的步骤：

1. 编写测试文件：需要在项目中创建一个测试文件夹，并在其中创建一个具有 `.fish` 后缀的文件，命名为 `test_foo.fish`，其中 `foo` 是你想要测试的函数名。
2. 导入测试框架：在测试文件的开头，使用 `source` 命令导入 `fish_tap` 测试框架。
3. 编写测试用例：使用 `begin` 和 `end` 命令包裹测试用例，并在用例中使用 `ill` 命令来断言测试结果是否符合预期。
4. 运行测试：在终端中使用 `fish test_foo.fish` 命令来运行测试文件，可以看到测试结果的输出。

下面是一个简单的例子：

```Fish Shell
source $fish_function_path[1]/fish_tap.fish

begin "sum函数"
  set result (sum 1 2)
  ill "结果应为3" $result
end
```

运行后，如果测试通过，会显示 "1 of 1 tests passed"，如果测试失败，则会显示具体的错误信息。

## 深入了解编写Fish Shell测试

除了基本的用法外，还有一些更高级的功能可以帮助我们编写更复杂的测试：

- 使用 `plan` 命令来规定测试用例的数量，防止测试漏掉某些用例。
- 使用 `set_up` 和 `tear_down` 命令来在每个测试用例的前后执行一些初始化和清理操作。
- 使用 `skip` 命令来跳过某个特定的测试用例。
- 使用 `negative` 命令来测试函数的异常情况。
- 使用 `require` 命令来在测试文件中引入其他的测试文件。

更详细的用法可以参考 `fish_tap` 的官方文档。

## 查看更多

- [Fish Shell官方网站](https://fishshell.com)
- [Fish Shell测试框架文档](https://github.com/fish-shell/fish-shell/blob/master/doc_src/test.md)
- [Fish Shell测试示例](https://github.com/fish-shell/fish-shell/blob/master/tests/builtin_math_tests.fish)

# 参考链接

- [Fish Shell官方文档 - 测试](https://fishshell.com/docs/current/tutorial.html#testing)
- [Fish Shell官方文档 - 关于测试框架](https://fishshell.com/docs/current/test.html)
- [Fish Shell开发者指南 - 测试框架](https://fishshell.com/docs/current/developer.html#testing-framework)