---
title:    "Ruby: 编写测试"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/writing-tests.md"
---

{{< edit_this_page >}}

# 为什么?

写测试，是软件开发中不可或缺的一个环节。通过编写测试代码，可以确保我们的软件在每次更改后仍然能够正常运行。同时，它也能够帮助开发者在编码过程中发现潜在的问题，并提高整体代码质量。

# 如何编写测试?

编写测试并不难，下面我们通过一些实例来解释如何使用Ruby进行测试。

首先，我们需要安装RSpec库。在终端输入以下命令即可：

```Ruby
gem install rspec
```

接着创建一个新的Ruby文件，命名为“calculator.rb”，包含两个方法：add和subtract。

```Ruby
def add(x, y)
  x + y
end

def subtract(x, y)
  x - y
end
```

现在，我们为这些方法编写测试代码。在同一目录下，创建一个新的RSpec测试文件，命名为“calculator_spec.rb”。

```Ruby
require_relative "calculator" # 导入我们刚刚创建的“calculator”文件

RSpec.describe "Calculator" do
  describe "add" do
    it "returns the sum of two numbers" do
      expect(add(1, 2)).to eq(3)
    end
  end

  describe "subtract" do
    it "returns the difference of two numbers" do
      expect(subtract(5, 3)).to eq(2)
    end
  end
end
```

现在我们可以运行测试文件来验证代码是否正确。在终端输入以下命令：

```Ruby
rspec calculator_spec.rb
```

如果一切顺利，你将会得到两个通过的测试案例。

# 深入了解

编写测试并不只是为了验证代码是否按预期运行。它也能帮助我们为未来的改动做好准备，避免不必要的错误。此外，编写测试也能够让我们更加自信地重构代码，从而提高整体代码质量。

总的来说，编写测试是一种良好的编程习惯，能够帮助我们更加高效地开发软件。

# 参考资料

* [RSpec官方网站](https://rspec.info/)
* [RSpec教程](https://www.tutorialspoint.com/rspec/index.htm)
* [Ruby编程语言官方网站](https://www.ruby-lang.org/en/)
* [Ruby文档](https://www.ruby-doc.org/)