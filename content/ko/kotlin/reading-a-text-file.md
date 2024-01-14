---
title:    "Kotlin: 텍스트 파일 읽기"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 읽는 것을 왜 즐겨 읽는지 설명하는 짧은 글입니다.

텍스트 파일을 읽는 것은 프로그래머에게 매우 중요한 기술입니다. 우리는 종종 여러 가지 웹 애플리케이션을 만들 때 텍스트 파일에 데이터를 저장하고, 읽어오고, 수정하는 작업을 수행합니다. 또한 여러분이 코드를 생성하거나 특정 목적을 위해 데이터를 처리하기를 원할 때 유용합니다.

# 방법

우리는 먼저 코틀린에서 텍스트 파일을 읽는 기본적인 방법을 살펴보겠습니다. 먼저 파일을 읽을 때 사용할 함수인 "readLines()"를 사용합니다. 이 함수는 텍스트 파일의 각 라인을 배열에 저장합니다. 우리는 "forEach()" 함수를 사용하여 각 배열 요소(즉, 각 라인)를 출력할 수 있습니다.

```Kotlin
val fileName = "textFile.txt"
val fileLines = File(fileName).readLines()
fileLines.forEach { println(it) }
```

위 코드는 "textFile.txt"이라는 파일에서 각 라인을 읽어오고, 각 라인을 println() 함수를 이용하여 출력하는 간단한 예제입니다. 아래는 코드를 실행한 결과입니다.

```
This is the first line.
This is the second line.
This is the third line.
```

파일의 모든 내용을 한 번에 읽어오지 않고, 한 라인씩 읽어온다는 것을 의미합니다. 이 함수는 각 라인을 읽어오기 때문에 파일의 크기와 관계없이 파일을 가볍게 처리할 수 있습니다.

# 깊게 파헤치기

이제 조금 더 깊게 들어가 보겠습니다. 일반 텍스트 파일을 처리하는 것 외에도, 코틀린에서는 CSV 파일, XML 파일, JSON 파일 등 다양한 형식의 파일도 읽고 처리할 수 있습니다. 이를 위해 다른 함수를 사용해야 합니다.

우리는 "BufferedReader()" 함수를 사용하여 파일을 읽습니다. 이 함수는 파일을 읽어오는 스트림을 생성합니다. 해당 스트림에 대한 참조를 가지고 있으면, 우리는 파일의 내용을 하나 하나 처리할 수 있습니다.

```Kotlin
val fileName = "csvFile.csv"
val reader: BufferedReader = File(fileName).bufferedReader()
val lines: List<String> = reader.readLines()
lines.forEach { println(it) }
```

위 코드는 CSV 파일인 "csvFile.csv"에서 각 라인을 읽어오고, 각 라인을 출력하는 예제입니다. 아래는 코드를 실행한 결과입니다.

```
Last Name, First Name, Address 1, Address 2, City, ZIP Code, State, Country
Lee, Jung-min, 123 Main St., Apt. 1, Seoul, 12345, South Korea
Smith, John, 555 Broadway, -, New York, 10001, USA
Kim, Ji-eun, 999 Park St., #200, Busan, 54321, South Korea
```

스트림 방식을 사용하면 파일의 모든 내용을 메모리에 한꺼번에 저장하지 않고, 내용을 점진적으로 처리할 수 있습니다. 따라서 대용량 파일을 다룰 때 매우 유용합니다.

# 또 다른 정보

- [Kotlin 텍스트 파일 다루는 방법](https://kotlinlang.org/docs/reference/idioms.html#reading-a-file)
- [Kotlin CSV 파일 다루는 방법](https://xlsxl.readthedocs.io/en/v0.2.2/io/csv/)
- [Kotlin JSON 파일 다루는 방법](https://github.com/json-path/JsonPath)