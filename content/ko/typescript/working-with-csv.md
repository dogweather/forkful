---
title:                "csv 파일과 함께 작업하기"
html_title:           "TypeScript: csv 파일과 함께 작업하기"
simple_title:         "csv 파일과 함께 작업하기"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/working-with-csv.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
CSV는 'comma-separated values(쉼표로 구분된 값)'의 약자로, 데이터를 쉽게 표현하기 위해 사용되는 파일 형식입니다. 프로그래머들은 CSV 파일 형식을 사용하여 데이터를 구조화하고 분석하는 등 다양한 작업에 활용할 수 있습니다.

## 방법:
아래는 TypeScript를 이용하여 CSV 파일을 다루는 예제 코드입니다. 적절한 출력 결과도 함께 포함되어 있습니다.

```TypeScript
// 'data' 변수에는 CSV 형식의 데이터가 저장되어 있습니다.
const data = "Name,Age,Location
John,25,New York
Sally,30,Los Angeles
Mike,35,Chicago";

// CSV 파싱 함수를 생성합니다.
function parseCSV(csv: string): string[][] {
  return csv.split('\n').map(row => row.split(','));
}

// 파싱 결과를 출력합니다.
const parsedData = parseCSV(data);
console.log(parsedData);
```

아래는 출력 결과입니다.

```
[
  [ 'Name', 'Age', 'Location' ],
  [ 'John', '25', 'New York' ],
  [ 'Sally', '30', 'Los Angeles' ],
  [ 'Mike', '35', 'Chicago' ]
]
```

## 깊이 파고들기:
CSV 파일 형식은 1972년에 처음으로 개발되었습니다. 현재는 Excel과 같은 프로그램을 통해 쉽게 다룰 수 있지만, 과거에는 데이터를 다루기 위해 많은 시간과 노력이 필요했습니다. CSV 파일 형식 외에도 JSON, XML 등 다양한 형식으로 데이터를 저장할 수 있지만, 간단한 구조와 쉽게 읽고 쓸 수 있다는 점에서 CSV는 여전히 많은 사용자들에게 유용한 형식입니다. 

사용된 함수인 `split()`과 `map()`에 대한 구체적인 설명은 [문서](https://developer.mozilla.org/ko/docs/Web/JavaScript/Reference/Global_Objects/Array/split)를 확인하시기 바랍니다.

## 관련 링크:
- [CSV 파일 형식 설명서](https://www.loc.gov/preservation/digital/formats/fdd/fdd000323.shtml)
- [CSV to JSON 변환 도구](https://www.csvjson.com/csv2json)