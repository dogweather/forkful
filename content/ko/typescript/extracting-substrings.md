---
title:    "TypeScript: 부분 문자열 추출"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

중국어 프로그래밍 블로그게시물

## 왜?

문자열에서 부분 문자열을 추출하는 것은 프로그래밍에서 자주 사용되는 기술입니다. 부분 문자열을 추출하면 특정 단어나 구를 찾고 분석하는 데 도움이 됩니다. 예를 들어, 이메일 주소에서 도메인을 추출하거나 텍스트에서 중요한 단어를 추출할 수 있습니다.

## 추출 방법

부분 문자열을 추출하는 가장 간단한 방법은 `substring()` 메서드를 사용하는 것입니다. 이 메서드는 시작 위치와 종료 위치를 지정하여 원하는 부분만을 추출할 수 있습니다. 다음은 TypeScript로 작성한 코드 예제입니다.

```TypeScript
const fullName = "홍길동"
const firstName = fullName.substring(0, 1)
console.log(firstName) // 출력 결과: 홍
```

위 코드에서는 `fullName` 변수에서 첫 번째 글자만을 추출하여 `firstName` 변수에 할당하고 있습니다. 추출된 부분 문자열은 `console.log()`를 사용하여 출력하였습니다.

`substring()` 메서드 뿐만 아니라 정규식을 사용하여도 부분 문자열을 추출할 수 있습니다. 예를 들어, `match()` 메서드를 사용하여 특정 패턴과 일치하는 부분을 추출할 수 있습니다. 다음은 이메일 주소에서 도메인을 추출하는 예제입니다.

```TypeScript
const email = "johndoe@example.com"
const domain = email.match(/@(.+)/)[1]
console.log(domain) // 출력 결과: example.com
```

위 코드에서는 정규식 `/@(.+)/`을 사용하여 `@` 다음에 나오는 모든 문자열을 추출하고 있습니다. 이 추출된 부분은 `[1]`을 사용하여 첫 번째 그룹을 가져오고 있습니다.

## 깊이 파고들기

부분 문자열 추출에 대해 깊이 파고들기 전에 중요한 개념인 인덱스를 이해해야 합니다. 인덱스는 컴퓨터에서 위치를 나타내는 숫자입니다. 프로그래밍에서는 문자열의 각 문자마다 인덱스가 할당되어 있으며 첫 번째 문자의 인덱스는 0부터 시작합니다.

현재 JavaScript에서는 `substring()` 메서드보다 더 다양한 기능을 제공하는 `slice()` 메서드를 사용하는 것이 권장됩니다. 이 메서드는 시작 인덱스와 종료 인덱스를 지정하는 것 외에도 음수 값을 사용하여 문자열의 끝부터 추출하는 것과 같은 유용한 기능을 제공합니다.

부분 문자열 추출에서는 오프셋과 길이를 다루는 것이 중요합니다. 오프셋은 시작 위치를 나타내는 인덱스이고 길이는 추출하려는 문자열의 길이를 나타냅니다.

## 관련 글

- [TypeScript 공식 문서](https://www.typescriptlang.org/docs/handbook/basic-types.html)
- [TypeScript 기본문법](https://brunch.co.kr/@inthewalter/2)
- [정규식 테스트 사이트](https://regexr.com/)