---
date: 2024-01-20 17:57:22.460847-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uB300\uCCB4\uB294 \uAE30\uC874\
  \ \uBB38\uC790\uC5F4\uC744 \uCC3E\uC544 \uB2E4\uB978 \uBB38\uC790\uC5F4\uB85C \uBC14\
  \uAFB8\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\
  \uC740 \uCF54\uB4DC \uC218\uC815, \uB370\uC774\uD130 \uC815\uC81C, \uC124\uC815\
  \ \uBCC0\uACBD \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\uB85C \uC774 \uC791\uC5C5\uC744\
  \ \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:55.457669-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9\uACFC \uB300\uCCB4\uB294 \uAE30\uC874 \uBB38\
  \uC790\uC5F4\uC744 \uCC3E\uC544 \uB2E4\uB978 \uBB38\uC790\uC5F4\uB85C \uBC14\uAFB8\
  \uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740\
  \ \uCF54\uB4DC \uC218\uC815, \uB370\uC774\uD130 \uC815\uC81C, \uC124\uC815 \uBCC0\
  \uACBD \uB4F1 \uB2E4\uC591\uD55C \uC774\uC720\uB85C \uC774 \uC791\uC5C5\uC744 \uD569\
  \uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## What & Why? (대체 무엇이며 왜 필요한가?)
텍스트 검색과 대체는 기존 문자열을 찾아 다른 문자열로 바꾸는 작업입니다. 프로그래머들은 코드 수정, 데이터 정제, 설정 변경 등 다양한 이유로 이 작업을 합니다.

## How to: (하는 법)
```Bash
# 'oldtext'를 'newtext'로 대체합니다.
sed -i 's/oldtext/newtext/g' filename

# 예시: file.txt 내의 'seoul'를 'busan'으로 대체
sed -i 's/seoul/busan/g' file.txt
```
실행 후 file.txt 안의 모든 'seoul' 문자열이 'busan'으로 변환됩니다.

## Deep Dive (심층 분석)
검색과 대체 기능은 Unix에서 제공하는 `sed` 스트림 편집기를 통해 구현됩니다. `sed`는 1973-74년에 개발되었고, 텍스트 처리에 강력한 도구입니다. `awk`, `grep`, `perl` 같은 다른 텍스트 처리 도구들도 있지만, `sed`는 간단한 텍스트 교체에 가장 많이 쓰입니다. `-i` 옵션은 파일 내용을 직접 수정하도록 합니다. 'g' 플래그는 전체 파일에 걸쳐 모든 일치 항목을 대체합니다.

## See Also (추가 자료)
- GNU sed manual: https://www.gnu.org/software/sed/manual/sed.html
- Regular expressions guide: https://www.regular-expressions.info/
- An introduction to awk: https://www.gnu.org/software/gawk/manual/gawk.html
- Learn more about grep: https://www.gnu.org/software/grep/manual/grep.html
