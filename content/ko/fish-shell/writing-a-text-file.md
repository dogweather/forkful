---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 작성은 데이터를 영구적으로 저장하는 방법이다. 프로그래머는 설정, 스크립트, 데이터 교환을 위해 이를 자주 실행한다.

## How to: (방법)
Fish Shell에서 텍스트 파일 만들기와 데이터 추가:

```Fish Shell
echo "안녕하세요, Fish 시작합니다!" > greetings.txt  # 파일 생성
cat greetings.txt  # 출력: 안녕하세요, Fish 시작합니다!

echo "Fish Shell을 배웁시다." >> greetings.txt  # 데이터 추가
cat greetings.txt  # 출력:
# 안녕하세요, Fish 시작합니다!
# Fish Shell을 배웁시다.
```

## Deep Dive (심층 분석)
텍스트 파일 쓰기는 유닉스 시대부터 있었다. `>` 는 새 파일을 만들거나 기존 파일을 대체하고, `>>`는 기존 파일에 추가한다. Fish Shell은 Bash와 다른 셸과 호환되는 방식으로 이 기능을 구현한다.

## See Also (참고 자료)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Learning the Fish Shell](https://fishshell.com/docs/current/tutorial.html)
