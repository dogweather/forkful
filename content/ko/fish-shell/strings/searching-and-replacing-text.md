---
date: 2024-01-20 17:58:10.280695-07:00
description: "\uC2E4\uD589 \uBC29\uBC95: (\"How to:\") Fish Shell\uC5D0\uC11C \uAC80\
  \uC0C9 \uBC0F \uAD50\uCCB4\uB294 `string replace` \uBA85\uB839\uC5B4\uB85C \uAC04\
  \uB2E8\uD558\uAC8C \uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC544\uB798 \uC608\uC2DC\
  \uB97C \uB530\uB77C \uD574\uBCF4\uC138\uC694."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.424115-06:00'
model: gpt-4-1106-preview
summary: "(\"How to:\") Fish Shell\uC5D0\uC11C \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294\
  \ `string replace` \uBA85\uB839\uC5B4\uB85C \uAC04\uB2E8\uD558\uAC8C \uD560 \uC218\
  \ \uC788\uC2B5\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## 실행 방법:
("How to:")

Fish Shell에서 검색 및 교체는 `string replace` 명령어로 간단하게 할 수 있습니다. 아래 예시를 따라 해보세요.

```Fish Shell
# 단순한 단어 교체
echo "Fish Shell is fun" | string replace "fun" "awesome"
```
출력:
```
Fish Shell is awesome
```

```Fish Shell
# 여러 파일에서 일괄 교체하기
for file in *.txt
    string replace -i "old_text" "new_text" $file
end
```

## 깊이 알아보기:
("Deep Dive")

초기 쉘 프로그램은 `sed`나 `awk` 같은 도구로 텍스트를 검색하고 교체했습니다. Fish Shell은 사용하기 쉬운 `string` 명령어를 내장해 이 일을 간편하게 해줍니다. 대체 명령어를 사용하면 인터페이스가 일관되거나 더 풍부한 기능을 제공하기도 합니다. `grep`은 검색만 할 때 주로 쓰이는 반면, `string replace`는 Fish Shell 자체 기능으로 더 손쉬운 문법을 제공합니다.

## 관련 자료:
("See Also")

- Fish Shell 공식 문서: [https://fishshell.com/docs/current/commands.html#string](https://fishshell.com/docs/current/commands.html#string)
- Unix `sed` 명령어: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Unix `grep` 명령어: [https://www.gnu.org/software/grep/manual/grep.html](https://www.gnu.org/software/grep/manual/grep.html)
