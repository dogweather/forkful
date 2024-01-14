---
title:    "Fish Shell: 텍스트 검색과 바꾸기"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 왜

텍스트를 검색하고 바꾸는 작업은 프로그래밍에서 매우 중요합니다. 코드에서 특정한 패턴을 찾고 바꾸는 것은 코드의 효율성을 높이고 버그를 예방하는 데 도움이 됩니다. 이를 통해 프로그램을 더욱 견고하고 유지보수하기 쉽게 만들 수 있습니다.

## 어떻게

텍스트를 검색하고 바꾸는 것은 Fish Shell에서 매우 간단합니다. 먼저 다음과 같이 `grep` 명령어를 사용하여 원하는 패턴을 찾습니다.

```Fish Shell
grep "특정한 패턴" 파일.txt
```

그리고 이 패턴을 원하는 결과로 바꾸는 것은 `sed` 명령어를 사용합니다.

```Fish Shell
sed -i "s/패턴/바꿀 내용/" 파일.txt
```

예를 들어, "안녕하세요"라는 문구를 "반가워요"로 바꾸고 싶다면 다음과 같이 입력합니다.

```Fish Shell
sed -i "s/안녕하세요/반가워요/" 파일.txt
```

그리고 `sed` 명령어를 실해하고 나서 `파일.txt`를 다시 열어보면 원하는 결과로 바뀐 것을 확인할 수 있습니다.

## 딥 다이브

`sed` 명령어의 옵션 중 하나인 `g`는 전체 패턴 검색 후 바꾸는 것을 의미합니다. 따라서 한 줄에 여러 번 해당 패턴이 나오는 경우에 유용하게 사용할 수 있습니다. 또한 `//` 사이에 사용된 정규표현식을 바꾸어 다양한 패턴도 한 번에 바꿀 수 있습니다.

또한 `sed` 명령어를 사용하여 파일의 특정 부분만 검색하고 바꾸는 것도 가능합니다. 예를 들어 `sed -i "1,5s/패턴/바꿀 내용/" 파일.txt`와 같이 입력하면 파일의 1~5번째 줄에서만 패턴을 바꿀 수 있습니다.

## See Also

- [Fish Shell 공식 문서](https://fishshell.com/docs/)
- [정규표현식을 이용한 검색 및 바꾸기](https://blog.naver.com/PostView.nhn?blogId=davidsa&logNo=220886706773)
- [패턴 매칭과 치환](https://unix.stackexchange.com/questions/392541/pattern-matching-and-substitution-sed-and-grep)