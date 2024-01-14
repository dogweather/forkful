---
title:                "Fish Shell: 부분 문자열 추출"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜

파일을 다룰 때 종종 사람들은 특정 부분을 추출하고 싶어합니다. 이를 위해 여러 가지 방법이 있지만, `substr` 함수는 특정 문자열에서 원하는 부분을 추출하는 데에 유용하게 사용될 수 있습니다.

## 어떻게

먼저, `substr`을 사용하기 전에 먼저 Fish Shell을 적극적으로 사용해야 합니다. 그 후 Fish Shell을 설치하고 사용할 수 있게 되면, `substr` 함수를 사용하여 원하는 부분을 추출할 수 있습니다.

```
Fish Shell에서 문자열 추출하기

$ set string "안녕하세요"

$ echo (substr -l 2 $string)
안녕
```

위의 예시에서 보듯이, `substr` 함수를 사용할 때는 먼저 추출하고자 하는 길이를 지정해야 합니다. `-l` 옵션을 사용하여 추출할 길이를 지정해줄 수 있습니다. 여기서는 `2`를 지정하여 "안녕"이 추출되었습니다.

## 깊게 들어가기

`substr` 함수는 추출할 문자열이 시작하는 위치를 지정하는 `-b` 옵션도 제공합니다. 기본값은 `0`으로, 문자열의 처음부터 시작하는 것을 의미합니다. 따라서 `-b` 옵션을 사용하여 추출하고자 하는 문자열의 시작 위치를 지정해줄 수 있습니다.

```
Fish Shell에서 문자열 추출하기

$ set string "첫번째부터 다섯번째까지의 문자열"

$ echo (substr -b 0 -l 5 $string)
첫번째
```

또한 `substr` 함수는 `-e` 옵션을 제공하여 추출하고자 하는 문자열의 끝 위치를 지정해줄 수도 있습니다. 이 옵션은 추출할 문자열의 길이가 아닌 끝 위치를 지정하는 것이기 때문에, `-l` 옵션과 함께 사용하여 추출하고자 하는 문자열의 길이를 지정해줘야 합니다.

```
Fish Shell에서 문자열 추출하기

$ set string "처음부터 여섯번째까지의 문자열"

$ echo (substr -b 0 -e 6 $string)
처음부
```

## 더 알아보기

`substr` 함수는 길이나 시작/끝 위치를 지정하는 옵션 외에도 다양한 옵션을 제공합니다. 또한 주어진 문자열이 아닌 파일에서도 문자열을 추출할 수 있는 `substr < FILE` 형태의 사용법도 있습니다. 더 많은 정보는 [Fish Shell 공식문서](https://fishshell.com/docs/current/cmds/substr.html)를 참고하세요.

## 연관 정보

* [Fish Shell 설치 방법](https://fishshell.com/)
* [Fish Shell 공식문서](https://fishshell.com/docs/current/)
* [Fish Shell의 유용한 기능들](https://github.com/fish-shell/fish-shell/wiki/Built-in-Functions)
* [Fish Shell의 고급 설정 및 사용법](https://en.wikipedia.org/wiki/Fish_(Unix_shell))