---
title:                "Fish Shell: 텍스트 검색 및 치환"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

"## 왜"

작성자님이 흔히 찾아봤던 "텍스트를 검색하고 바꾸는 방법"에 대해 궁금한 이유는 무엇일까요? 검색과 바꾸기는 문서를 쉽게 수정하고 업데이트하는 데에 필수적인 작업입니다. Fish Shell을 사용하면 더욱 쉽고 효율적으로 이 작업을 수행할 수 있습니다. 그래서 오늘은 Fish Shell에서 텍스트를 검색하고 바꾸는 방법에 대해 알아보겠습니다.

"## 사용 방법"

이제 실제로 코드를 작성해보겠습니다. 먼저 Fish Shell에 내장된 `sed` 명령어를 사용하여 텍스트를 검색하고 바꾸는 예제를 살펴보겠습니다.

```Fish Shell
sed -i 's/원래 텍스트/바뀐 텍스트/g' 파일이름
```

위의 예제에서 `s`는 substitute(바꾸기)를 의미하며, `g`는 global(전체)을 의미합니다. 즉, 파일에서 모든 원래 텍스트를 바뀐 텍스트로 바꾸라는 명령입니다. `파일이름` 대신 `.`을 입력하면 현재 폴더의 모든 파일에서 검색하고 바꿀 수 있습니다.

또 다른 예제를 살펴보겠습니다. 만약 파일에서 A로 시작하는 모든 단어를 B로 바꾸고 싶다면 다음과 같이 입력하면 됩니다.

```Fish Shell
sed -i 's/\bA/B/g' 파일이름
```

여기서 `\b`는 word boundary(단어 경계)를 의미합니다. 즉, A가 단어의 시작일 때만 바꾸도록 설정하는 명령입니다.

"## 깊이 들어가보기"

위에서 언급한 `sed` 명령어 외에도 Fish Shell에는 텍스트를 검색하고 바꾸는 더 다양한 방법이 있습니다. 예를 들어, `grep` 명령어를 사용하여 파일에서 원하는 내용을 찾은 후, `awk`를 사용해 특정 부분만 추출한 뒤 그 부분을 다시 바꾸는 작업도 가능합니다.

Fish Shell 공식 문서나 인터넷에서 더 많은 정보를 찾아보시면 더 많은 방법을 알 수 있습니다. 텍스트 검색과 바꾸기는 초보자와 전문가 모두에게 필수적인 기술이므로 꼭 익혀두는 것이 좋습니다.

"## 관련 자료"

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell를 이용한 특정 단어로 시작하는 파일 찾기](https://www.shelltunis.com/2018/07/fish-shell-find-files-starting-with.html)
- [Fish Shell 명령어 모음](https://devhints.io/fish)