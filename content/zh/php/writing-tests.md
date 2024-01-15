---
title:                "编写测试"
html_title:           "PHP: 编写测试"
simple_title:         "编写测试"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/writing-tests.md"
---

{{< edit_this_page >}}

画A PHP(最新版本)编程文章，为普通话读者使用非正式的语气和简洁的风格。文章分别有三个在标题下明确的部分：「##为什么」，「##如何」，「##深入探讨」。

为什么：写测试的目的，不超过2句话。

如何：在「```PHP ... ```」的代码块中，举出编码范例和输出样本。

深入探讨：关于编写测试的更深层信息。

这篇文章没有「结论」部分。文章以Markdown标题「参见」结尾，以链接列表形式列出。

#为什么

编写测试是一种重要的开发实践，它可以帮助我们确保代码质量、修复错误和避免未来的问题。通过编写测试，我们可以更快速、更准确地开发代码，并且在添加新功能或进行重构时更有信心。

#如何

##安装PHPUnit
首先，我们需要安装PHPUnit来运行PHP测试。使用Composer是最简单的方法，只需在命令行中运行以下命令：

```PHP 
composer require --dev phpunit/phpunit
```

##编写测试
创建一个测试类，并添加 `@test` 标签来标识测试方法。在测试方法中，我们将编写一些断言来验证代码的预期行为是否与实际结果一致。例如：

```PHP 
class GreetingTest extends PHPUnit\Framework\TestCase
{
  /** @test */
  public function it_returns_hello_world()
  {
    $greeting = new Greeting();
    $message = $greeting->sayHello();
    
    $this->assertEquals('Hello World!', $message);
  }
}
```

##运行测试
要运行测试，只需在命令行中运行 `phpunit` 命令，并指定测试类的路径：

```PHP 
./vendor/bin/phpunit tests/
```

##深入探讨

编写测试的目的是为了让我们能够快速、准确和可靠地验证代码是否按照预期工作。通过编写测试，我们可以更好地组织我们的代码，并且可以在添加新功能或者修改现有功能时安全地进行重构。

另外，编写测试还可以帮助我们发现隐藏的Bug，并且它们也可以作为一种文档形式，帮助其他开发人员理解代码的预期行为。

总的来说，编写测试可以提高代码质量和可靠性，从而减少错误和维护成本。

#参见

- [PHPUnit官方文档](https://phpunit.de/documentation.html)
- [为什么写测试？](https://medium.com/@person/reasons-to-write-tests-f330a0972091)
- [如何编写更好的测试？](https://medium.com/@person/tips-for-writing-better-tests-5ec9fda351aa)