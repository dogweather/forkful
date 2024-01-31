---
title:                "正規表現の使用"
date:                  2024-01-19
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"

category:             "Java"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
（なぜとは？）
正規表現は文字列をパターンで操作するためのツール。プログラマはデータ検証、検索、テキスト処理の効率化のために使う。

## How to:
（やり方）
```Java
import java.util.regex.*;

public class RegexExample {
    public static void main(String[] args) {
        String text = "今日は2023年4月13日です";
        String patternString = "\\d{4}年\\d{1,2}月\\d{1,2}日";
        
        Pattern pattern = Pattern.compile(patternString);
        Matcher matcher = pattern.matcher(text);
        
        while(matcher.find()) {
            System.out.println("日付発見: " + matcher.group());
        }
    }
}
```
出力:
```
日付発見: 2023年4月13日
```

## Deep Dive
（深掘り）
正規表現は1960年代に登場。`java.util.regex` パッケージがJavaの正規表現機能を提供。PerlやPythonも強力な正規表現機能を持つ。パフォーマンスを重視するなら文字列操作メソッドや外部ライブラリを検討するべき。

## See Also
（関連情報）
- [Java Regex ドキュメント](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/package-summary.html)
- [正規表現チュートリアル](https://www.regular-expressions.info/tutorial.html)
- [Apache Commons Lang - StringUtils](https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html)
