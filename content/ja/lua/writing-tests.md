---
title:                "テストの書き方"
html_title:           "Lua: テストの書き方"
simple_title:         "テストの書き方"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/lua/writing-tests.md"
---

{{< edit_this_page >}}

「## テストを書く理由とやり方」

テストを書くとは、自分が書いたプログラムが正しく動くかどうかを確かめる作業です。プログラマーたちは、バグを発見し修正するためにテストを行います。

「## やり方：」

```Lua
-- 「こんにちは」をプリントする関数
function say_hello()
  print("こんにちは")
end

say_hello()
```

実行結果：「こんにちは」

```Lua
a = 5
b = 10

-- aとbの和を計算する関数
function sum()
  return a + b
end

print(sum())
```

実行結果：15

「## 深堀り」

プログラミングの歴史的文脈では、テストはソフトウェア開発の重要な段階です。また、テストを行う代わりに、プログラマーたちは「デバッグ」や「リファクタリング」を行うこともできます。テストを書く際は、テストデータやエラー処理にも注意しましょう。

「## 関連リンク」

- Lua公式ドキュメント：https://www.lua.org/docs.html
- Luaのテストフレームワーク：https://github.com/bluebird75/luaunit