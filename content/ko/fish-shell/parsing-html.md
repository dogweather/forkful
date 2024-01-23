---
title:                "HTML 파싱"
date:                  2024-01-20T15:31:52.139126-07:00
html_title:           "Arduino: HTML 파싱"
simple_title:         "HTML 파싱"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (무엇이며 왜?)
HTML 파싱은 HTML 문서의 구조를 분석해서 데이터를 추출하는 과정입니다. 프로그래머들은 데이터를 재사용하거나 웹 콘텐츠를 분석하기 위해 이 작업을 합니다.

## How to:
Fish에서 HTML을 파싱하려면, 주로 외부 도구를 호출합니다. 여기에 `pup`를 사용하는 예제를 보여드리죠.

```Fish Shell
# pup을 설치합니다 (brew를 이용한 macOS 기준)
brew install pup

# 간단한 HTML 파일을 만든 후 파싱합니다.
echo "<html><body><p>Hello, Fish!</p></body></html>" | pup 'p text{}'

# 출력
Hello, Fish!
```

`pup`은 CSS 선택자를 사용해 HTML 요소를 편리하게 추출할 수 있습니다.

## Deep Dive (심층 탐구)
HTML 파싱은 초기 월드와이드웹의 시절부터 필요했습니다. 복잡성이 증가함에 따라 더 강력하고 빠른 도구들이 개발되었죠.

Fish Shell 자체에는 HTML 파싱 기능이 내장되어 있지 않습니다. 하지만 `pup`, `hxselect` 또는 `xmllint`와 같은 외부 프로그램을 Fish 스크립트에서 호출하여 사용합니다.

Fish의 장점은 쉘 스크립트 내에서 다른 프로그램과의 연계가 매우 간단하다는 점입니다. 다른 언어로 작성된 파싱 도구를 쉽게 조합할 수 있어 유연한 해결책을 제공합니다.

HTML 파싱 기술 중에는 정규 표현식이 자주 언급되지만, 이 방법은 복잡하거나 예외적인 경우에 적합하지 않아 권장하지 않습니다.

## See Also (더 보기)
- pup: https://github.com/ericchiang/pup
- hxselect: http://man7.org/linux/man-pages/man1/hxselect.1.html
- xmllint: http://xmlsoft.org/xmllint.html
- Fish Shell 공식 문서: http://fishshell.com/docs/current/index.html

이 글이 HTML 파싱에 대한 간단한 도입 부분을 이해하는 데 도움이 되기를 바랍니다. '더 보기' 섹션의 도구들을 더 살펴보시면 Fish Shell 환경에서 강력한 HTML 파싱 스킬을 키울 수 있습니다.
