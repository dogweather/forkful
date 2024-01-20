---
title:                "创建临时文件"
html_title:           "Kotlin: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 什么和为什么？

创建临时文件是在保存不确定持久性必要但需要短时间内保存数据的情况下，快捷、动态地在内存或磁盘上生成文件的编程操作。程序员之所以要做这个，是因为临时文件提供了一个简便的方式，来在程序执行期间的不同阶段之间传递和整个程序访问的数据。

## 如何操作：

Elixir并没有内建的临时文件创建函数，但通过调用Erlang的 `:file.mkstemp` 函数，我们可以轻易达成。交互式操作可见如下： 

```elixir
iex> {:ok, {path, file}} = :file.mkstemp("/tmp/tempfile_")  
{:ok,
  {"/tmp/tempfile__rug4as6h7rvb.ljcq6sy2fr2fv805",
   {:file_descriptor, :prim_file, {:fd, :fd_user, 18}}}}
```

操作成功后，我们得到一对元祖，其中包含文件路径和一个已经开启的读写模式的文件描述符。

## 深入了解

### 历史上下文
临时文件起源于Unix操作系统，现已广泛用于各种编程语言和操作系统之中。对于Elixir这样的基于Erlang虚拟机(beam)运行的语言来说，直接继承了Erlang强大的文件处理能力。

### 可选方案
Elixir内部没有提供临时文件的创建，你可以选择调用 `:file.mkstemp` 或者使用类似 `Exfile` 的第三方库。

### 实现细节
Erlang的 `:file.mkstemp` 函数首先接受一个包含 `"XXXXXX"`的路径参数并产生一个唯一的文件名，然后在给定的目录中以读写模式创建并打开文件。这个过程是原子的，保证了文件的唯一性和安全性。

## 参见

- [Elixir Docs](https://hexdocs.pm/elixir/File.html): 更多关于Elixir工作中文件操作的信息。
- [Erlang Docs](http://erlang.org/doc/man/file.html#mkstemp-1): Erlang原生的 `:file.mkstemp` 函数的文档。