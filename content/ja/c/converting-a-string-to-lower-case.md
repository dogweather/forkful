---
title:    "C: 文字列を小文字に変換する"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することについて、1-2文で説明します。このプロセスには、さまざまなケースで文字列を比較する必要がある場合に役立つからです。

## 方法
"```C
#include <stdio.h>
#include <string.h>

int main()
{
	char myString[] = "CONVERT TO LOWER CASE";
	printf("Original string: %s\n", myString);

	// convert string to lower case
	for (int i = 0; i < strlen(myString); i++) {
		myString[i] = tolower(myString[i]);
	}
	printf("Converted string: %s\n", myString);

	return 0;
}
```"
### 出力:
```console
Original string: CONVERT TO LOWER CASE
Converted string: convert to lower case
```
この例では、C言語の標準ライブラリである"string.h"と"stdio.h"を使用して、文字列を小文字に変換する方法を示しています。まず、元の文字列を表示し、次に文字列内のすべての文字を小文字に変換し、最後に変換後の文字列を表示しています。

##ディープダイブ
文字列を小文字に変換するには、まず文字列内のすべての文字をASCIIコードに基づいてUnicodeを使用して比較します。次に、小文字との差を計算し、文字を小文字に変換できるように新しい文字を生成します。このプロセスをすべての文字に繰り返し実行し、最終的に小文字に変換された新しい文字列を生成します。

## みてみる
- [C言語で文字列を小文字に変換する方法](https://www.programiz.com/c-programming/examples/lowercase-string)
- [文字列を小文字に変換するアルゴリズムの詳細](https://www.geeksforgeeks.org/convert-whole-string-lower-case-upper-case/)
- [UnicodeとASCIIについて詳しく学ぶ](https://www.ibm.com/developerworks/library/ws-unicode/)

## もっと見る
- [C言語で文字列操作をマスターする方法](https://www.tutorialspoint.com/cprogramming/c_string_manipulation.htm)
- [C言語での標準ライブラリの有用な機能](https://www.tutorialspoint.com/c_standard_library/index.htm)