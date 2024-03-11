---
date: 2024-01-20 17:58:10.280695-07:00
description: "(\"What & Why?\") \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\
  \uB294 \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uBB38\uC7A5\uC744 \uB2E4\uB978 \uAC83\uC73C\
  \uB85C \uCC3E\uC544 \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uAC1C\uC120\uD558\uAC70\uB098\
  \ \uC624\uB958\uB97C \uC218\uC815\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744\
  \ \uC790\uC8FC \uC2E4\uD589\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.756805-06:00'
model: gpt-4-1106-preview
summary: "(\"What & Why?\") \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4\uB294\
  \ \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uBB38\uC7A5\uC744 \uB2E4\uB978 \uAC83\uC73C\uB85C\
  \ \uCC3E\uC544 \uBC14\uAFB8\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uCF54\uB4DC\uB97C \uAC1C\uC120\uD558\uAC70\uB098 \uC624\
  \uB958\uB97C \uC218\uC815\uD558\uAE30 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC790\
  \uC8FC \uC2E4\uD589\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## 무엇과 왜?
("What & Why?")

텍스트 검색 및 교체는 특정 단어나 문장을 다른 것으로 찾아 바꾸는 과정입니다. 프로그래머들은 코드를 개선하거나 오류를 수정하기 위해 이 작업을 자주 실행합니다.

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
