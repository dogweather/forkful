---
title:                "Haskell: 一時ファイルの作成"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# なぜ一時ファイルを作成するのか？

一時ファイルは、一時的にデータを保存するのに便利です。例えば、プログラムの実行中に一時的な変数を保存したり、複数のプログラム間でデータを共有したりすることができます。

## 作り方

一時ファイルを作成するには、Haskellの標準ライブラリである`System.IO.Temp`を使用します。まず、一時ファイルを作成したいディレクトリを指定します。

```Haskell
import System.IO.Temp (withSystemTempFile)

withSystemTempFile "tmp/" $ \fp h -> do
  -- ここにファイル作成のコードを記述します
  -- 作成したファイルを使用したコードを記述します
  hClose h -- ファイルをクローズします
```

上記のコードでは、`tmp/`ディレクトリに`withSystemTempFile`関数でファイルを作成し、ファイルハンドル`h`を取得しています。このファイルハンドルを使用して、ファイルにデータを書き込んだり、読み込んだりすることができます。

## 詳細解説

一時ファイルを作成する際、ファイル名を指定する必要はありません。また、ファイルが作成されると同時にファイルハンドルも取得できるので、すぐにファイルを操作することができます。さらに、ファイルを使用し終わった後は、自動的にクリーンアップされるため、メモリリークなどの問題を気にすることなく使うことができます。

# これを知っといたら便利！

一時ファイルを作成する方法を知ることで、プログラムの作成やデータ処理をよりスムーズに行うことができます。ぜひ試してみてください。

# おわりに

一時ファイルの作成方法についてご紹介しました。一時ファイルを作成する際に役立つ情報をお伝えできたかと思います。ぜひ参考にしてください。

# 関連記事

- [Haskellの標準ライブラリ - System.IO.Temp](https://hackage.haskell.org/package/base/docs/System-IO-Temp.html)
- [一時ファイルを作成する方法まとめ](https://qiita.com/toshimaru/items/a29287b147e1f031943a)
- [一時ファイルの利用メリットとデメリット](https://yagince.hatenablog.com/entry/2013/05/24/122148)