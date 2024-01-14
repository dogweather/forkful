---
title:    "Elixir: 编写测试"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么

写测试是编程中不可或缺的一部分。它可以帮助我们保证我们的代码质量，减少潜在的错误和故障，并且提供更好的可读性和可维护性。

## 如何

下面是一个简单的例子来展示如何使用Elixir编写测试：

```elixir
# 一个简单的函数，用来计算两个数的和
def add(a, b) do
  a + b
end

# 测试用例
test "add函数应正确计算两个数的和" do
  assert add(2, 3) == 5 # 断言add(2, 3)的结果应为5
end
```

运行测试：

```
mix test
```

输出应该为：

```
Compiling 1 file (.ex)
…

Finished in 0.01 seconds
1 test, 0 failures
```

通过编写测试，我们可以确保add函数能够正确计算两个数的和，并且在未来的修改中保持正确。

## 深入探讨

编写测试可以为我们提供更多的保障和安全感，但是要想确保测试的有效性，还需要注意以下几点：

- 测试应该覆盖代码的不同路径和边界情况，以保证代码的鲁棒性。
- 当代码发生变化时，测试也应该相应地更新，以保证测试的有效性。
- 良好的测试应该能够隔离出发生错误的具体部分，帮助我们更快地定位和解决问题。
- 除了单元测试，还应该结合使用集成和端到端测试来确保整个系统的稳定性。

总的来说，编写测试是一项很重要的开发实践，它可以帮助我们开发出更加可靠和健壮的代码。

## See Also

- [Elixir官方文档 - 测试](https://hexdocs.pm/elixir/1.12.0/testing-with-elixir.html)
- [《Elixir程序设计 - 测试](https://alanmacd.gitee.io/elixir/introduction/testing.html)