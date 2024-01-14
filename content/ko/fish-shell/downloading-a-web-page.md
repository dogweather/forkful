---
title:                "Fish Shell: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜 
웹 페이지를 다운로드하는 이유는 사람들이 원하는 정보를 쉽게 얻기 위해서입니다. 웹 페이지는 수많은 정보를 담고 있으며, 이를 자유롭게 다운로드하여 자신의 목적에 맞게 가공할 수 있습니다.

## 다운로드 하는 방법 
Fish Shell을 사용하여 웹 페이지를 다운로드하는 방법은 간단합니다. 먼저 `curl` 명령어를 사용하여 다운로드할 웹 페이지의 URL을 입력합니다. 그리고 `> 파일명.html`을 추가하여 해당 파일로 저장합니다. 아래는 실제 예시입니다.

```Fish Shell
curl https://example.com > example.html
```

위 명령어를 실행하면 `example.html` 파일이 생성됩니다. 이 파일을 열어보면 정확히 해당 웹 사이트의 내용을 포함하고 있습니다.

## 깊게 파고들기 
웹 페이지를 다운로드하는 것은 매우 기본적이지만, 좀 더 깊게 파고들어볼 수도 있습니다. 예를 들어 특정 부분만 다운로드하거나, 다운로드한 내용을 특정 포맷으로 가공하는 등의 작업을 할 수 있습니다. 이를 위해서는 Fish Shell 내장 변수를 사용하여 다운로드한 내용에 접근하고, 조건문과 반복문을 활용할 수 있습니다.

## See Also 
- [curl 공식 문서](https://curl.se/docs/manual.html)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
- [웹 스크래핑에 대한 자세한 설명](https://www.dataquest.io/blog/web-scraping-tutorial-python/)