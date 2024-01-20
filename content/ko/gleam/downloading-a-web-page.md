---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

웹 페이지 다운로드는 인터넷에서 웹 페이지의 내용을 사용자의 컴퓨터에 불러오는 것을 말합니다. 프로그래머들이 이를 수행하는 이유는 웹 크롤링, 데이터 스크래핑 등 다양한 목적으로 웹 페이지의 정보가 필요하기 때문입니다.

## 어떻게 하나요:

Gleam을 이용해 웹 페이지를 다운로드하는 코드와 그 결과를 아래에 보여드립니다.

```Gleam
pub fn download() {
  let url = "https://www.example.com";
  let response = httpc::send(method::Get, url, [], [], default())
                      .await
                      .gleam_expect("Failed to download the web page");
  log(response.body)
}
```
위 코드의 실행 결과는 다음과 같습니다:

```Gleam
"<html>... 웹 페이지의 HTML 내용 ...</html>"
```

## 딥다이브:

웹 페이지 다운로드는 웹의 초기 시절부터 있던 기능으로, 웹의 역사와 발전과 함께 성장해왔습니다. 현재는 다양한 언어와 라이브러리를 이용해 비교적 간단히 웹 페이지를 다운로드할 수 있습니다. 

Gleam에서 기본 httpc 모듈을 통해 제공하는 get 메서드를 사용해서 웹 페이지를 다운로드하고 있습니다. 이 방법 외에도, 직접 HTTP 요청을 만들어 보내는 방법 등 다양한 대안적 방법이 존재합니다. 

다운로드의 구현 세부 사항은 비동기를 활용하여 서버로부터 응답을 기다리는 동안 다른 작업을 수행할 수 있도록 되어있습니다.

## 참조:

- [Gleam Documentation](https://gleam.run/docs)
- [Gleam HTTP Library](https://github.com/gleam-lang/http)