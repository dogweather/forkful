---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何 & なぜ?
デバッグ出力とはプログラムの実行中に変数の値を表示することで、プログラマーはこれを使ってプログラムの振る舞いを理解し、バグを見つけて修正します。

## 使い方
PHPでは、`print_r()` または `var_dump()` を使うことでデバッグ出力を行うことができます。
例えば:
```PHP
$testArray = array("Apple", "Banana", "Cherry");
var_dump($testArray);
```
出力結果は下記のように表示されます:
```PHP
array(3) {
  [0]=>
  string(5) "Apple"
  [1]=>
  string(6) "Banana"
  [2]=>
  string(6) "Cherry"
}
```
## 詳細情報
### デバッグ出力の歴史
デバッグ出力はプログラミングの初期から存在し、問題の特定と解決のための重要なツールでした。

### 他の方法
`print_r()` と `var_dump()` の他に、PHPでは `debug_zval_dump()`,`var_export()`, `print()` などもデバッグ出力に使用できます。

### 実装の詳細
`var_dump()` は指定された変数の構造と値を表示します。`print_r()` は配列の構造をレイアウトした文字列を表示しますが、`var_dump()` が提供する情報量が多いため、複雑なデータ構造のデバッグ出力にはこちらを推奨します。

## 参考情報
[PHP公式ドキュメンテーション](https://php.net/manual/ja/language.types.array.php) の中の '配列の説明' セクションや関連関数の説明を確認すると、さらに詳しい情報を得ることができます。