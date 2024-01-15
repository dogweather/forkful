---
title:                "문자열 연결하기"
html_title:           "Gleam: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜?
문자열을 연결하는 것에 참여하는 이유는 다양합니다. 첫째, 문자열 연결은 데이터 처리, 템플릿 생성 및 문서 작성과 같은 다양한 작업에 필수적인 요소입니다. 둘째, 문자열을 연결하는 것은 코드를 더 재사용 가능하고 유지 보수가 용이하게 만드는 방법 중 하나입니다.

## 어떻게?
"```Gleam ... ```" 코드 블록 안에 코딩 예제와 샘플 출력을 포함하였습니다.

### 두 문자열 연결하기
```Gleam
let string1 = "안녕하세요, "
let string2 = "글림입니다."
let result = string1 ++ string2
```
**결과:**
```
안녕하세요, 글림입니다.
```

### 문자열과 변수 연결하기
```Gleam
let name = "제이슨"
let greet = "안녕하세요, "
let result = greet ++ name
```
**결과:**
```
안녕하세요, 제이슨
```

### 숫자와 문자열 연결하기
```Gleam
let number = 2021
let message = "년에 글림을 시작해보세요!"
let result = number |> toString ++ message
```
**결과:**
```
2021년에 글림을 시작해보세요!
```

## 조금 더 깊은 내용
문자열을 연결하기 위해 Gleam에서는 `++` 연산자를 사용합니다. 이 연산자는 왼쪽에 있는 문자열과 오른쪽에 있는 문자열을 하나의 새로운 문자열로 결합하는 역할을 합니다. 또한 `toString` 함수를 사용하여 숫자를 문자열로 바꿔줄 수 있습니다.

## 더 자세한 내용은 ...

## 참조
- [Gleam 공식 문서](https://gleam.run/documentation/)
- [Gleam 개발자 블로그](https://dev.to/gleam_lang)
- [Gleam 커뮤니티 포럼](https://discuss.gleam.run/)