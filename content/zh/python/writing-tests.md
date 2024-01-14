---
title:    "Python: 编写测试"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/python/writing-tests.md"
---

{{< edit_this_page >}}

## 为什么要编写测试

编写测试是软件开发过程中非常重要的一步。通过编写测试，我们可以提高代码的质量和稳定性，并减少出现错误的可能性。测试也可以帮助我们快速定位和修复bug，节省开发时间和成本。因此，无论是作为一个专业的程序员，还是一个学习Python的爱好者，编写测试都是必不可少的。

## 如何编写测试

编写测试的方法很简单，我们只需要使用Python自带的`unittest`模块即可。首先，我们需要导入`unittest`模块：

```Python
import unittest
```

接着，我们可以创建一个测试类，该类继承`unittest.TestCase`类，并编写我们需要测试的函数：

```Python
class MyTestCase(unittest.TestCase):
    # 测试函数以test开头
    def test_add(self):
        result = 1 + 2
        self.assertEqual(result, 3) # 使用断言来判断结果是否正确
```

然后，我们可以使用`unittest.main()`来运行我们的测试：

```Python
if __name__ == '__main__':
    unittest.main()
```

运行结果如下所示：

```
.
----------------------------------------------------------------------
Ran 1 test in 0.000s

OK
```

如果我们想要测试多个函数，只需要在测试类中添加更多的测试函数，并使用不同的断言来判断结果是否正确。

## 深入了解编写测试

除了使用断言来判断结果是否正确，我们还可以使用`setUp()`和`tearDown()`方法来分别进行测试前的准备和测试后的收尾工作。我们也可以使用`skip()`来跳过某些测试，以及使用`assertRaises()`来判断函数是否会抛出异常。此外，我们还可以使用`mock`模块来模拟测试中的依赖项。

## 查看更多资源（See Also）

- [unittest模块文档](https://docs.python.org/3/library/unittest.html)
- [Python官方教程](https://docs.python.org/3/tutorial/index.html)
- [Python Unittest：如何编写单元测试](https://www.jianshu.com/p/75935dff7fde)