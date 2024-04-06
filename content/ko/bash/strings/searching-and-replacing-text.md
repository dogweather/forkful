---
date: 2024-01-20 17:57:22.460847-07:00
description: "How to: (\uD558\uB294 \uBC95) \uC2E4\uD589 \uD6C4 file.txt \uC548\uC758\
  \ \uBAA8\uB4E0 'seoul' \uBB38\uC790\uC5F4\uC774 'busan'\uC73C\uB85C \uBCC0\uD658\
  \uB429\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.136784-06:00'
model: gpt-4-1106-preview
summary: "(\uD558\uB294 \uBC95) \uC2E4\uD589 \uD6C4 file.txt \uC548\uC758 \uBAA8\uB4E0\
  \ 'seoul' \uBB38\uC790\uC5F4\uC774 'busan'\uC73C\uB85C \uBCC0\uD658\uB429\uB2C8\uB2E4\
  ."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

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
