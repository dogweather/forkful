---
title:                "エラー処理"
aliases:
- ja/go/handling-errors.md
date:                  2024-02-03T17:58:12.190603-07:00
model:                 gpt-4-0125-preview
simple_title:         "エラー処理"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？

Go言語でのエラー処理は、プログラムのエラー状態を認識し、対応することを意味します。プログラマは、予期せぬ状況からアプリケーションが適切に回復できるようにするためにエラー処理に取り組みます。これにより、より堅牢で信頼性の高いソフトウェアが実現されます。

## 方法

Goでは、`error`型を使用してエラー処理を明示的に管理します。失敗する可能性のある関数は、最後の戻り値としてエラーを返します。このエラー値が`nil`かどうかをチェックすることで、エラーが発生したかどうかがわかります。

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("value must be 100 or less")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Error:", err)
    } else {
        fmt.Println("Result:", result)
    }
    
    // エラーを適切に処理する
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Error:", anotherErr)
    } else {
        fmt.Println("Result:", anotherResult)
    }
}
```

上記のコードのサンプル出力：
```
Error: value must be 100 or less
Result: 100
```

この例では、`Compute`関数は計算された値またはエラーのいずれかを返します。呼び出し元は、`err`が`nil`でない場合にエラーを処理します。

## 深掘り

Goのエラー処理アプローチは意図的にシンプルで型安全であり、エラーの明確なチェックを必要とします。この概念は、JavaやPythonのような言語で見られる例外ベースのエラー処理と対照的で、エラーは例外ハンドラによって捕捉されない限り、呼び出しスタックを上に伝搬します。Goチームは、エラーの明示的な処理によって、プログラマが発生した場所で直接エラーに対処することを強制するため、コードがより明確で信頼性が高くなると主張しています。

ただし、多くのエラーを起こしやすい操作を含む複雑な関数では、このパターンによりコードが冗長になる可能性があるという批判もあります。応答として、Goの新しいバージョンでは、元のエラー情報を失うことなくエラーにコンテキストを提供しやすくする、より洗練されたエラー処理機能（例：エラーのラッピング）が導入されています。また、check/handleのような新しいエラー処理メカニズムに関する提案もコミュニティで見られますが、これらは私の最後の更新時点でまだ議論中です。

Goのエラー処理哲学は、プログラムの正常なフローの一部としてエラーを理解し、計画することを強調しています。このアプローチは、特に複雑なケースのためのエラー処理を合理化する代替パターンやライブラリが存在するものの、Goの組み込み`error`型が言語のエラー処理の基礎であり続けることを確実にしながら、より回復力があり予測可能なソフトウェアの開発を促進します。
