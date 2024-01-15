---
title:                "ディレクトリが存在するかどうかを確認する"
html_title:           "Elm: ディレクトリが存在するかどうかを確認する"
simple_title:         "ディレクトリが存在するかどうかを確認する"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## なぜ

ディレクトリの存在をチェックする必要があるか、というと、プログラミングでは時に特定のディレクトリが存在するかどうかを確認する必要が出てくる場合があります。例えば、ディレクトリ内に特定のファイルが存在する場合だけ処理を実行する、といった場面で有用です。そのため、ディレクトリの存在をチェックすることは、より効率的なプログラミングを行うために重要なスキルです。

## 方法

ディレクトリの存在をチェックするには、Elmの`Directory.exists`関数を使用します。例えば、次のコードでは、`Directory.exists`関数を使用してユーザーのホームディレクトリが存在するかどうかをチェックしています。

```Elm
Directory.exists "user/home" 
```

もしホームディレクトリが存在する場合は、`True`というBool値が返されます。もし存在しない場合は、`False`が返されます。プログラムの流れを制御するために、このBool値を使用することができます。

ディレクトリの存在をチェックすると同時に、ディレクトリの作成や削除などの処理も行うことができます。例えば、次のコードでは、`Directory.exists`関数の結果に応じて、ホームディレクトリを作成しています。

```Elm
if Directory.exists "user/home" == False then
    Directory.create "user/home"
else
    -- ディレクトリがすでに存在する場合の処理
```

## 深堀り

`Directory.exists`関数は、ディレクトリのパスを受け取り、Bool値を返す単純な関数です。しかし、Elmには`Directory`モジュール以外にも様々なモジュールが存在し、より高度なファイルやディレクトリ操作を行うことができます。

例えば、`FileSystem`モジュールを使用すると、ファイルやディレクトリの作成や削除、リネームなどの処理を簡単に行うことができます。また、`Http`モジュールを使用すると、リモートのサーバー上のファイルやディレクトリの存在をチェックすることもできます。

重要なことは、Elmには様々なモジュールが用意されているため、ディレクトリの存在をチェックするには`Directory.exists`関数だけではなく、より適した機能を持つモジュールを使用することもできるということです。

## 参考情報

- Elm公式ドキュメント（日本語）: https://guide.elm-lang.jp/
- Elmのモジュール一覧（英語）: https://package.elm-lang.org/packages/elm/core/latest/
- Elmのモジュール一覧（日本語）: https://guide.elm-lang.jp/