---
title:    "Gleam: 부분 문자열 추출"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 부분 문자열을 추출하는 이유는 사용자가 자신이 필요한 정보만을 추출하여 처리하거나 조작하기 위해서입니다. 예를 들어, 특정 패턴이나 키워드를 포함한 문자열을 찾거나 바꾸는 등의 작업을 수행할 수 있습니다. 부분 문자열 추출은 문자열 처리에 유용한 기능 중 하나이며 Gleam 프로그래밍에서도 자주 활용됩니다.

## 하우 투

부분 문자열 추출은 `String.substr()` 함수를 사용하여 쉽게 구현할 수 있습니다.

```
Gleam stdout.print(String.substr("Hello Gleam!", 6, 5))
```

위의 코드는 "Gleam"이라는 부분 문자열을 추출하고 콘솔에 출력하는 예시입니다. `String.substr()` 함수는 첫 번째 매개변수로 추출할 문자열, 두 번째 매개변수로 추출을 시작할 인덱스, 세 번째 매개변수로 추출할 문자열의 길이를 받습니다.

출력:

```
Gleam
```

부분 문자열 추출은 부분 문자열의 길이가 문자열의 총 길이보다 작을 때 유용합니다. 예를 들어, 이메일 주소에서 도메인 부분만을 추출하거나, 파일명에서 확장자 부분만을 추출하는 등의 작업에 활용할 수 있습니다.

## 딥 다이브

`String.substr()` 함수를 비롯한 여러 문자열 처리 함수들은 Gleam 표준 라이브러리의 `String` 모듈에서 제공됩니다. 이 모듈은 문자열에 대한 다양한 작업을 수행할 수 있는 함수들을 유용하게 제공해줍니다. 사용 가능한 모든 함수와 이들의 사용 방법은 공식 문서를 참고하시면 좋습니다.

## 봐도 되요

- [Gleam 표준 라이브러리 공식 문서](https://gleam.run/documentation/standard-library#string)
- [Gleam 공식 웹사이트](https://gleam.run)
- [Gleam 공식 GitHub 저장소](https://github.com/gleam-lang/gleam)