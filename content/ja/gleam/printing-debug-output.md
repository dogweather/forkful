---
title:    "Gleam: デバッグ出力を出力する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ
デバッグ出力を行う理由は、コードの動作やエラーの原因を特定するためです。コンソールに出力された情報を確認することで、プログラムの実行を追跡し、問題を解決することができます。

## 方法
デバッグ出力を行うには、 `debug / 1` 関数を使用します。この関数には、出力したい値を渡す必要があります。下記の例では、変数 `name` の値を出力しています。

```Gleam
debug(name)
```

このコードを実行すると、コンソールに `name` の値が表示されます。出力された情報を確認することで、 `name` の値を確認することができます。

## ディープダイブ
デバッグ出力を行う際には、出力する情報を選ぶことが重要です。不必要な情報を出力してしまうと、デバッグが困難になることがあります。また、デバッグ出力を行う際には、システムがパフォーマンスに影響を与えないよう注意が必要です。適切な箇所にデバッグ出力を配置することで、問題の特定と解決をより早く行うことができます。

## 参考リンク
- https://gleam.run/documentation/
- https://gleam.run/documentation/library/debug