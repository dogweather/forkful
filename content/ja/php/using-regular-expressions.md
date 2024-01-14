---
title:                "PHP: 正規表現の使用"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ正規表現を使用するのか

正規表現は、テキストに特定のパターンを検索したり置換したりする際に非常に便利です。これは、多くのプログラミング言語でサポートされている強力なツールですが、PHPでは特に役立ちます。

## 正規表現の使い方

正規表現を使用するには、まず `preg_match()`関数を使用してパターンとテキストを比較する必要があります。例えば、以下のコードブロックはテキスト内のすべての電話番号を検索する方法を示しています。

```PHP
// テキスト内の電話番号を検索
$text = "私の電話番号は080-1234-5678です。";
$pattern = "/[0-9]{3}-[0-9]{4}-[0-9]{4}/"; // 「3桁の数字-4桁の数字-4桁の数字」のパターンを表す
preg_match($pattern, $text, $matches); // マッチした結果が$matchesに格納される

// 検索結果を出力
echo "電話番号: " . $matches[0]; // $matchesの0番目にはマッチした結果が入っている
```

実行結果:
```
電話番号: 080-1234-5678
```

他にも `preg_replace()`関数を使用することで、テキスト内のパターンを置換することができます。また、正規表現にはさまざまな特殊文字やメタ文字があるので、使い方についてよく学ぶ必要があります。

## 正規表現の詳細

正規表現では、`[ ]`を使用してパターンを定義することができます。1つの`[ ]`内に含まれる文字は任意の1文字とマッチします。例えば、`[aeiou]`というパターンは、テキスト内の母音の1文字にマッチします。

また、`{ }`を使用することで繰り返しを表現することができます。例えば、`[0-9]{3}`というパターンは、3回繰り返される数字にマッチします。

さらに、`^`や`$`といった特殊文字を使用することで、パターンの開始や終了を表現することができます。例えば、`^[a-z]$`というパターンは、1文字だけのアルファベットにマッチします。

これらはほんの一例であり、正規表現にはさまざまな使い方があります。詳細な解説はPHPのドキュメントや他のリソースを参考にしてください。

## 参考リンク

- [PHPの正規表現：preg_match()](https://www.php.net/manual/ja/function.preg-match.php)
- [PHPの正規表現：preg_replace()](https://www.php.net/manual/ja/function.preg-replace.php)
- [改行コードを正規表現で検索する](https://www.php.net/manual/ja/reference.pcre.pattern.syntax.php)
- [正規表現チュートリアル](https://www.php.net/manual/ja/regexp.reference.tutorial.php)

## 関連リンク

- [PHPの正規表現を使ってデータを正しくバリデーションする方法](https://www.website.com/article1)
- [正規表現のパターン