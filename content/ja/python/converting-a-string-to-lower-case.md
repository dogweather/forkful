---
title:                "「文字列を小文字に変換する」"
html_title:           "Python: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

文字列を小文字に変換する理由は、多種多様です。例えば、データの整形やメールアドレスの入力時の検証、文字列の比較などが挙げられます。

## How To

文字列を小文字に変換するには、`lower()`メソッドを使用します。以下のコード例を参考にしてください。 

```Python
name = "JOHN"
lower_name = name.lower()
print(lower_name)
```

実行結果は`john`となります。また、日本語の場合も同様に`lower()`メソッドを使用して変換することができます。 

```Python
name = "太郎"
lower_name = name.lower()
print(lower_name)
```

実行結果は`太郎`のままです。これは、日本語では大文字と小文字の区別がないためです。 

## Deep Dive

`lower()`メソッドは、文字列を小文字に変換するだけでなく、アクセント記号やその他の特殊文字も変換します。また、アルファベット以外の文字はそのまま変換せずに残します。 

例えば、`lower()`メソッドは以下のように動作します。 

- `É`を`é`に変換する
- `Ç`を`ç`に変換する
- `ü`を`ü`のままにする
- `令`を`令`のままにする

文字列を比較する場合にも、小文字に統一することでより正確な判定ができます。 

## See Also

- [Python string methods](https://www.w3schools.com/python/python_ref_string.asp)
- [Unicode characters in Python](https://docs.python.org/3/howto/unicode.html)