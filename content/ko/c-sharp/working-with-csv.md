---
title:                "CSV 파일 작업하기"
html_title:           "C#: CSV 파일 작업하기"
simple_title:         "CSV 파일 작업하기"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

CSV를 다루는 것은 일반적으로 쉼표로 구분된 값 파일로서 데이터를 저장하고 전송하기 위해 사용됩니다. 프로그래머들은 보다 효율적이고 간단한 방법으로 데이터를 처리하기 위해 CSV 파일을 사용합니다.

## 방법:

### CSV 파일 읽기:

```C#
using(var reader = new StreamReader("파일경로"))
{
  while(!reader.EndOfStream)
  {
    var line = reader.ReadLine();
    var values = line.Split(','); // 쉼표를 구분자로 사용하여 값을 분리
    foreach(var value in values)
    {
      // 데이터 처리
    }
  }
}
```
### CSV 파일 쓰기:

```C#
using(var writer = new StreamWriter("파일경로"))
{
  var values = new List<string>(){"값1", "값2", "값3"};
  var line = string.Join(",", values); // 값들을 쉼표로 구분하여 한 줄로 결합
  writer.WriteLine(line);
}
```

## 깊이있게 알아보기:

### 역사적 배경:

CSV 파일 형식은 1972년 나외바가 개발한 Edwin Yamauchi의 개발한 구조를 기반으로 만들어졌습니다. 당시에 사용되었던 제품 모델 플레이트(PDP-11)에서는 데이터를 저장하기 위한 펴미터 파일 시스템이 없었기 때문에, 데이터를 쉼표로 구분하여 파일에 저장한 것이 CSV의 아버지라고 할 수 있습니다.

### 다른 대안:

CSV 파일을 다루는 대안으로는 JSON, XML 등이 있습니다. 각 형식의 장단점을 비교하여 프로젝트의 요구에 맞게 적합한 형식을 선택해야 합니다.

### 구현 내용:

CSV 파일을 읽기 위해서는 StreamReader 클래스를, 쓰기 위해서는 StreamWriter 클래스를 사용합니다. 또한 Split 메서드를 사용하여 쉼표로 구분된 값을 분리하거나, Join 메서드를 사용하여 자료형을 문자열로 결합할 수 있습니다.


## 관련 자료:

- [CSV 파일 형식](https://en.wikipedia.org/wiki/Comma-separated_values)
- [CSV를 다루는 방법](https://docs.microsoft.com/en-us/visualstudio/data-tools/load-and-save-data-as-a-csv-file)
- [나외바와 CSV](https://www.itworld.co.kr/news/128704)
- [데이터 형식 비교](https://medium.com/on-coding/data-format-comparison-json-vs-xml-vs-csv-1497262ad055)