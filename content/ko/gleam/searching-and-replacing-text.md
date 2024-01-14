---
title:    "Gleam: 텍스트 검색 및 교체하기"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜: 
만일 당신이 텍스트를 검색하고 바꾸는 작업에 참여하고 싶다면, 그 이유는 매우 간단합니다. 당신의 코드에서 오탈자나 잘못된 문법을 수정하고, 더 효율적인 코딩을 위해 특정 텍스트를 일괄적으로 변경하기 위해서입니다.

## 방법:
텍스트를 검색하고 바꾸는 작업을 Gleam으로 어떻게 할 수 있는지 살펴보겠습니다. 아래의 코드 예제는 간단한 텍스트 검색 및 치환 작업을 보여줍니다.

```Gleam
import gleam/re/replace
import gleam/io

// 초기 텍스트
let text = "안녕, Gleam 독자여!"

// "안녕"을 "안녕하세요"로 변경
let replaced = replace.regex(text, "안녕", "안녕하세요")

// 변경된 텍스트 출력
io.print(replaced)
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
안녕하세요, Gleam 독자여!
```

위의 예제에서는 단순한 문장을 대상으로 작업하였지만, Gleam의 강력한 정규식 기능을 사용하여 더 복잡한 텍스트 검색 및 치환 작업도 가능합니다.

## 깊이 들어가기:
텍스트 검색 및 치환 작업을 위해 Gleam의 정규식 기능을 제대로 활용하는 방법에 대해 더 알아보겠습니다. Gleam은 PCRE(Perl Compatible Regular Expressions) 라이브러리를 사용하기 때문에 광범위한 정규식 패턴을 지원합니다. 또한 캡쳐 그룹이나 후방참조 같은 고급 기능도 사용할 수 있습니다.

예를 들어, 아래의 코드는 주민등록번호에서 뒷자리를 마스킹하는 작업을 보여줍니다.

```Gleam
import gleam/re/replace
import gleam/io

// 주민등록번호 초기 값
let ssn = "801202-1234567"

// 마스킹할 부분을 정규식으로 지정
let regex = "\\d{6}(?=\\d{7})"

// 정규식을 사용하여 뒷자리 마스킹
let masked = replace.regex(ssn, regex, "******")

// 마스킹된 주민등록번호 출력
io.print(masked)
```

위의 코드를 실행하면 다음과 같은 출력이 나옵니다.

```
801202-******
```

더 많은 정규식 사용 예제와 Gleam의 다른 기능들을 통해 좀 더 심도 있는 텍스트 검색 및 치환 작업을 수행할 수 있습니다. 

## 참고:
- <https://gleam.run/documentation/standard-libraries/regex/>
- <https://gleam.run/documentation/tutorials/regex/>
- <https://gleam.run/documentation/tutorials/lists/>

---
# 참고:
- <https://gleam.run/documentation/standard-libraries/regex/>
- <https://gleam.run/documentation/tutorials/regex/>
- <https://gleam.run/documentation/tutorials/lists/>