---
title:                "정규 표현식 사용하기"
aliases: - /ko/typescript/using-regular-expressions.md
date:                  2024-02-03T19:18:36.883131-07:00
model:                 gpt-4-0125-preview
simple_title:         "정규 표현식 사용하기"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
정규 표현식, 또는 regex는 프로그래밍에서 강력한 패턴 매칭 및 검색 도구입니다. 프로그래머들은 사용자 입력 검증, 텍스트 검색, 문자열 조작 등의 작업에 regex를 사용하는데, 이는 효율적이며 다양하게 활용될 수 있기 때문입니다.

## 어떻게 사용하나:

TypeScript에서 정규 표현식을 일반적인 작업에 어떻게 사용하는지 살펴봅시다.

```TypeScript
// 이메일 주소를 위한 정규 표현식 패턴 정의
const emailPattern = /\S+@\S+\.\S+/;

// 문자열이 이메일 패턴과 일치하는지 테스트
const email = "user@example.com";
console.log(emailPattern.test(email)); // 출력: true

// 문자열에서 숫자 찾아 바꾸기
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // 출력: "Item # costs $#"

// 캡처 그룹을 사용해 문자열에서 특정 부분 추출하기
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, month, day, year] = datePattern.exec(data) || [];
console.log(month, day, year); // 출력: "April" "10" "2021"
```

## 깊이 들어가기

1950년대에, 수학자 스티븐 클리니는 정규 표현식을 정규 언어를 나타내는 모델로 설명했으며, 이는 나중에 컴퓨터 과학에서 필수적이 되었습니다. 시간이 지나, regex는 텍스트를 다루는 프로그래밍에서 어디에나 존재하게 되었습니다.

정규 표현식은 문자열 작업을 위한 스위스 군용 칼이지만, 대안이 없는 것은 아닙니다. 작업의 복잡성에 따라, 때때로 `includes()`, `startsWith()`, `endsWith()`와 같은 문자열 메서드나, 심지어 라이브러리를 사용한 파싱이 더 낫습니다. 예를 들어, 복잡한 JSON 문자열을 regex로 파싱하는 것은 악몽이 될 수 있습니다—대신 JSON 파서를 사용하세요.

구현과 관련하여, JavaScript와 TypeScript에서의 regex는 ECMAScript 언어 사양을 기반으로 합니다. 내부적으로, 엔진은 패턴을 효율적으로 매치하기 위해 상태 기계를 사용합니다. 정규 표현식 연산은 특히 잘못 작성된 패턴으로 인해 성능 측면에서 비용이 많이 들 수 있다는 점을 유의해야 합니다— "재앙적인 백트래킹"을 조심하세요.

## 참고자료

- MDN Web Docs 정규 표현식: [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101: 정규 표현식 패턴을 테스트하고 디버깅하는 도구 [Regex101](https://regex101.com/)
- 심층 이해를 위한 "정규 표현식 마스터하기" 책: [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
