---
date: 2024-01-20 17:41:38.021634-07:00
description: "\uD328\uD134\uACFC \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C\
  \uD558\uAE30\uB294 \uD2B9\uC815 \uBB38\uC790 \uB610\uB294 \uBB38\uC790\uC5F4\uC744\
  \ \uCC3E\uC544\uC11C \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC815\uC81C, \uC785\uB825\
  \ \uD615\uC2DD \uB9DE\uCD94\uAE30, \uB610\uB294 \uD544\uC694 \uC5C6\uB294 \uC815\
  \uBCF4\uB97C \uBC30\uC81C\uD560 \uB54C \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\
  \uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.456398-06:00'
model: gpt-4-1106-preview
summary: "\uD328\uD134\uACFC \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C\uD558\
  \uAE30\uB294 \uD2B9\uC815 \uBB38\uC790 \uB610\uB294 \uBB38\uC790\uC5F4\uC744 \uCC3E\
  \uC544\uC11C \uC81C\uAC70\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4."
title: "\uD328\uD134\uC5D0 \uC77C\uCE58\uD558\uB294 \uBB38\uC790 \uC0AD\uC81C"
weight: 5
---

## How to: (방법)
```Bash
# 문자 제거 예시: 'apple'에서 'p' 제거하기
$ echo "apple" | tr -d 'p'
ale

# 문자열 제거 예시: 'banana-123'에서 숫자 제거하기
$ echo "banana-123" | sed 's/[0-9]//g'
banana-

# 패턴으로 일치하는 전체 문자열 제거하기
$ my_string="Hello World 123"
$ echo ${my_string// *[0-9]*/}
Hello
```
위 코드는 'tr'과 'sed' 명령어, 그리고 Bash 패턴 치환을 사용하여 문자와 문자열을 제거하는 방법을 보여줍니다.

## Deep Dive (심층 분석)
문자 제거 기능은 Unix/Linux의 초기부터 사용되었습니다. 'tr' 명령어는 문자 집합을 변환하거나 삭제하는 데 쓰이며, 'sed'는 텍스트를 처리하는 스트림 편집기로, 복잡한 패턴 매칭과 치환이 가능합니다. Bash 자체에도 문자열 처리 기능이 내장되어 있어 패턴 매칭과 치환을 할 수 있습니다. `tr`은 단순 문자 변환에 적합하고, `sed`는 고급 텍스트 처리에 강력합니다. Bash 패턴 치환은 스크립트 내부에서 빠르게 작업하고자 할 때 유용합니다.

## See Also (더 보기)
- [GNU 'sed' Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [GNU 'tr' Manual](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
- Bash 문자열 조작에 관한 [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
