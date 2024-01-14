---
title:                "PHP: テストを書く"
simple_title:         "テストを書く"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-tests.md"
---

{{< edit_this_page >}}

「なぜ」PHPプログラミングでテストを書くのか

テストを書くことは、コードの品質を保つことに役立ち、バグを早期に発見することができます。また、テストはコードの一貫性を保つことができ、チームでのコラボレーションを容易にすることができます。

「やり方」

テストを書く方法を説明します。以下のコードを参考にしてください。

```PHP
<?php
function add($num1, $num2) {
    return $num1 + $num2;
}

// テストを書く
assert(add(2, 3) === 5);
assert(add(5, 10) === 15);

echo "テストがパスしました。";
```

上記のように、`assert()`関数を使用してテストを書くことができます。テストがパスすると、「テストがパスしました。」というメッセージが表示されます。

「深堀り」

テストを書く際には、コードカバレッジと呼ばれる概念にも注目する必要があります。コードカバレッジとは、テストがコードのどれだけをカバーしているかを示す指標です。高いコードカバレッジを保つことで、コードの品質を改善することができます。

また、テストがパスすることだけでなく、エラーケースや例外処理のテストも行うことが重要です。これらのテストは、実際のバグを早期に発見することができます。

「関連情報を見る」

- [PHPUnit公式ドキュメント (日本語)](https://phpunit.de/documentation.html)
- [テストを書くことの意義について (日本語)](https://qiita.com/fall_avenue/items/fac97973c2f56ba35b55)
- [テスト駆動開発の方法 (日本語)](https://codezine.jp/article/detail/3803)