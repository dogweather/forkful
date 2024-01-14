---
title:                "Elixir: ディレクトリが存在するかどうかをチェックする"
simple_title:         "ディレクトリが存在するかどうかをチェックする"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリが存在するかどうかをチェックする理由は、アプリケーションのパフォーマンスを向上させるためです。存在しないディレクトリを参照することは、エラーを引き起こし、アプリケーションの実行を妨げる可能性があります。そのため、ディレクトリが存在するかどうかを事前にチェックすることは重要なステップです。

## 方法

下記のコードブロックを参考に、ディレクトリが存在するかどうかをチェックする方法を学びましょう。

```Elixir
def check_directory(path) do
  case File.read_dir(path) do
    {:ok, _} -> IO.puts("Directory exists")
    {:error, _} -> IO.puts("Directory does not exist")
  end
end
```

上記のコードでは、`File.read_dir`を使用してディレクトリを読み込み、その結果を`case`文で処理します。ディレクトリが存在する場合は、`:ok`アトムが返され、存在しない場合は、`:error`アトムが返されます。`{:ok, _}`パターンマッチが成功した場合は、ディレクトリが存在することを示すメッセージを出力します。

## ディープダイブ

ディレクトリが存在するかどうかをチェックする際、厳密に存在するかどうかをチェックするか、または曖昧なチェックを行うかを検討する必要があります。厳密にチェックする場合は、`File.read_dir`の代わりに、より確実にディレクトリをチェックする`File.stat`関数を使用することができます。

また、ディレクトリのパスがユーザーが入力した場合は、改ざんされる可能性があるため、入力値を慎重に扱う必要があります。`Path.wildcard`関数を使用すると、不正なパスに対して安全に処理することができます。

## その他の情報

さらに詳しい情報をお探しの場合は、以下のリンクを参考にしてください。

[File.read_dirドキュメント](https://hexdocs.pm/elixir/File.html#content)

[File.statドキュメント](https://hexdocs.pm/elixir/File.html#stat/1)

[Path.wildcardドキュメント](https://hexdocs.pm/elixir/Path.html#wildcard/1)

## それでは、Happy coding!