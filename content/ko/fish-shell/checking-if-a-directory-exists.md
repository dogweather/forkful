---
title:                "Fish Shell: 디렉토리가 존재하는지 확인하기"
simple_title:         "디렉토리가 존재하는지 확인하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 왜

어떤 디렉토리가 존재하는지 확인하는 것이 중요한 이유는 사용자가 프로그래밍을 할 때 디렉토리의 존재 유무에 따라 다른 동작을 취해야 할 수도 있기 때문입니다.

## 어떻게

```Fish Shell
if test -d "/path/to/directory";
    echo "해당 디렉토리가 존재합니다."
else
    echo "해당 디렉토리가 존재하지 않습니다."
end
```

위의 코드를 실행하면 디렉토리가 존재하는지 여부에 따라 다른 문구가 출력됩니다.

## 딥 다이브

Fish Shell에서 디렉토리의 존재 여부를 확인하기 위해서는 `test` 명령어를 사용하면 됩니다. 이 명령어의 `-d` 옵션을 통해 해당 경로가 디렉토리인지 확인할 수 있습니다. 만약 디렉토리가 아니라면 오류가 발생하게 됩니다.

만약 Fish Shell이 아닌 다른 쉘을 사용하신다면 `test -d` 명령어 대신 `ls` 명령어를 사용할 수도 있습니다. 디렉토리가 존재할 경우 해당 디렉토리의 내용이 출력되고, 존재하지 않을 경우 오류가 발생합니다.

## 더 알아보기

이 외에도 Fish Shell에서 사용할 수 있는 디렉토리 관련 명령어에 대해서는 공식 문서를 참고해보세요.

## 더 읽어보기

- [Fish Shell 공식 문서 (영문)](https://fishshell.com/docs/current/index.html)
- [Fish Shell 설치 방법 (한국어)](https://blog.hangulize.org/2016/23930514154)
- [Fish Shell 스크립트 예제 (한국어)](https://d2.naver.com/helloworld/162063)