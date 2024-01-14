---
title:    "Fish Shell: 표준 에러 출력하기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

작성자가 표준 오류에 쓰는 것에 대해 이해하려고 한다면, 그것은 프로그래밍에서 오류를 디버깅하고 문제를 해결하는 데 매우 유용합니다.

## 어떻게

파일을 표준 오류에 쓰기 위해 Fish Shell에서는 `echo` 명령을 사용할 수 있습니다. 예를 들어, 다음과 같이 작성하십시오.

```Fish Shell
echo "이것은 표준 오류입니다" >&2
```

위의 코드는 "이것은 표준 오류입니다"를 표준 오류에 출력합니다.

## 깊게 파보기

표준 오류를 사용하면 프로그램이 실행되는 동안 오류 메시지를 출력할 수 있습니다. 또한 오류 메시지를 저장하는 파일을 생성하거나 네트워크로 보내는 등의 추가적인 기능을 수행할 수도 있습니다. 따라서 표준 오류를 적절하게 활용하면 프로그램의 문제를 어디서 발생했는지 빠르게 파악할 수 있으며, 보다 효율적으로 디버깅할 수 있습니다.

## 더 알아보기

- [공식 Fish Shell 문서](https://fishshell.com/docs/current/index.html)
- [Shell 스크립트 작성하기](https://tonybai.com/2015/05/12/writing-shell-scripts/)
- [Shell 스크립팅 - 표준 입출력](https://wiki.kldp.org/KoreanDoc/html/ShellScriptingRef-Small/x383.html#U347) 

## 참고

- [마크다운 사용법](https://www.markdownguide.org/basic-syntax/)