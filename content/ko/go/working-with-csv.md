---
title:                "Go: csv 처리하기"
simple_title:         "csv 처리하기"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/working-with-csv.md"
---

{{< edit_this_page >}}

## 왜?

CSV는 데이터를 쉽고 간단하게 저장하고 전송하기 위한 매우 유용한 파일 형식입니다. 따라서 CSV를 다룰 수 있는 Go 언어를 배우는 것은 매우 유익한 일일 것입니다.

## 어떻게?

CSV 파일을 열고 데이터를 읽고 쓰는 방법을 살펴보겠습니다. 우리는 `encoding/csv` 패키지를 사용할 것입니다. 먼저, CSV 파일을 열어서 쉼표(,)로 구분된 데이터를 읽어오겠습니다.

```Go
package main

import (
    "encoding/csv"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("data.csv")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    reader := csv.NewReader(file)
    // 첫 번째 줄은 데이터의 제목이기 때문에 무시합니다.
    reader.Read()

    // 나이, 이름 순으로 데이터를 읽어옵니다.
    records, err := reader.ReadAll()
    if err != nil {
        panic(err)
    }

    // 읽어온 데이터를 출력해봅니다.
    for _, record := range records {
        fmt.Println(record[0], record[1])
    }
}
```

위 코드를 실행하면 `data.csv` 파일에서 나이와 이름 데이터를 읽어 출력할 수 있습니다.

```
23 Alice
27 Bob
```

CSV 파일에 데이터를 쓰는 것도 간단합니다. 우리는 `csv.Writer`를 사용하여 데이터를 CSV 형식으로 작성할 수 있습니다.

```Go
...

writer := csv.NewWriter(os.Stdout)
// 첫 번째 인자는 CSV 파일에 쓰일 데이터의 제목입니다.
writer.Write([]string{"Name", "Age"})
writer.Write([]string{"Alice", "23"})
writer.Write([]string{"Bob", "27"})
writer.Flush()

...
```

위 코드를 실행하면 아래와 같은 CSV 형식의 데이터를 출력할 수 있습니다.

```
Name,Age
Alice,23
Bob,27
```

CSV 파일을 다루는 방법은 이렇게 간단합니다! 여러분들도 한 번 해보시길 바랍니다.

## 딥 다이브

CSV 파일을 다룰 때 유의해야 할 몇 가지 사항들이 있습니다.

먼저, 데이터의 양이 많을 경우에는 메모리를 많이 차지할 수 있으니 `csv.Reader` 대신 `csv.Scanner`를 사용하는 것이 좋습니다. `csv.Scanner`는 한 번에 한 줄씩 데이터를 읽어오므로 충분한 메모리 공간을 절약할 수 있습니다.

또한 `csv.Writer`에서는 `Flush()` 함수를 호출해줘야만 모든 데이터가 파일에 쓰여집니다. `Flush()`를 호출하지 않은 채 프로그램이 종료되면 데이터가 파일에 적절히 쓰여지지 않을 수 있습니다.

## 또 보기

- [공식 Go 블로그](https://go.dev/blog)
- [A Tour of Go](https://go-tour-kr.appspot.com)
- [Go 언어 공식 문서](https://golang.org/doc/)