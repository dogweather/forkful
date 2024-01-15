---
title:                "웹 페이지 다운로드"
html_title:           "Bash: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜?

웹 페이지를 다운로드하는 이유는 간단합니다. 인터넷에서 원하는 정보를 얻고 싶기 때문입니다. 

## 어떻게 하나요?

```Bash
# 웹 페이지 다운로드 예시
curl -O https://www.example.com/page.html
```
위의 코드는 웹 페이지를 다운로드하는 가장 간단한 방법입니다. "curl"이라는 프로그램을 사용하여 원하는 웹 페이지의 주소를 입력하고 "-O" 옵션을 사용하면 해당 페이지를 현재 디렉토리에 다운로드할 수 있습니다.

이 외에도 "wget"이라는 다른 프로그램을 사용하여 웹 페이지를 다운로드할 수 있습니다. 아래의 예시 코드를 참고해보세요.

```Bash
# 웹 페이지 다운로드 예시
wget https://www.example.com/page.html
```

다운로드한 웹 페이지를 열어보면 웹 브라우저에서 보는 것과 동일한 내용을 확인할 수 있습니다.

## 딥 다이브

위의 예시 코드는 가장 간단한 웹 페이지 다운로드 방법입니다. 하지만 실제로는 더 복잡한 다운로드 방식이 존재합니다. 예를 들어, 로그인이 필요한 페이지를 다운로드하거나 웹 서버에서 제공하는 API를 사용하는 경우에는 다른 방식으로 다운로드를 진행해야 합니다.

이 경우에는 "curl"이나 "wget"을 사용하여 웹 페이지를 다운로드하는 것이 아니라 프로그램을 직접 작성해야 합니다. 이를 위해서는 Bash 쉘 스크립트나 Python과 같은 프로그래밍 언어를 사용할 수 있으며, 웹 페이지 다운로드와 함께 필요한 작업을 추가적으로 수행할 수 있습니다.

## 관련 링크

- [Bash 공식 홈페이지](https://www.gnu.org/software/bash/)
- [Bash 스크립트 작성 방법](https://www.lifewire.com/write-for-loop-bash-script-2200574)
- [curl 사용 방법](https://www.lifewire.com/curl-command-tutorial-2618098)