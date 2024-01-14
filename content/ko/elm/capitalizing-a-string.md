---
title:    "Elm: 문자열 대문자로 바꾸기"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## 왜

문자열을 대문자로 바꾸는 것에 대해 궁금하신가요? 이 글에서는 Elm 프로그래밍에서 문자열을 대문자로 바꾸는 이유와 그 방법에 대해 알려드릴 것입니다.

## 어떻게

문자열을 대문자로 바꾸는 방법은 간단합니다. 다음과 같이 `String.toUpper` 함수를 사용하면 됩니다.

```Elm
import String

Strign.toUpper "hello" -- "HELLO"
```

위의 예시에서 `String.toUpper` 함수는 문자열을 대문자로 바꿔주는 역할을 합니다. 따라서 "hello"라는 문자열을 입력하면 "HELLO"라는 결과가 출력됩니다.

## 딥 다이브

문자열을 대문자로 바꾸는 방법을 더 자세히 알아보겠습니다. Elm에서는 기본적으로 `String` 라이브러리를 제공합니다. 따라서 `import` 구문을 사용하여 `String` 모듈을 불러와야 합니다.

`String` 모듈에는 `toUpper` 함수 외에도 `toLower`, `endsWith`, `startsWith` 등 다양한 함수가 있습니다. 이들 함수들은 모두 문자열을 다루는데 유용합니다. 더 자세한 정보는 [Elm 공식 문서](https://package.elm-lang.org/packages/elm/core/latest/String)를 참고해주세요.

## 더 알아보기

자세한 내용을 알아보기 위해 다음의 링크들을 참고해주세요.

- [Elm 공식 홈페이지](https://elm-lang.org/)
- [Elm 코리아 커뮤니티 블로그](https://elm-korea.github.io/)
- [Elm 코리아 Slack 커뮤니티](https://elm-korea.slack.com/)