---
title:                "웹 페이지 다운로드하기"
html_title:           "Bash: 웹 페이지 다운로드하기"
simple_title:         "웹 페이지 다운로드하기"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하나요?

웹 페이지 다운로드란 특정 웹사이트의 내용을 HTML 형식의 파일로 저장하는 것을 말합니다. 프로그래머들이 이를 사용하는 이유는 웹사이트의 데이터를 분석하거나, 웹 크롤링을 통해 다양한 정보를 수집하기 위해서입니다.

## 어떻게 사용할까요?

다음은 Elixir를 사용한 웹 페이지 다운로드 예제입니다. 

```Elixir
#HTTPoison 라이브러리 추가
defp deps do
  [{:httpoison, "~> 1.8"}]
end

#URL을 다운로드
def download_url() do
  case HTTPoison.get("http://example.com") do
    {:ok, %HTTPoison.Response{body: body, status_code: 200}} ->
      {:ok, body}
    {:ok, %HTTPoison.Response{status_code: code}} ->
      {:error, "Unexpected status code: #{code}"}
    {:error, %HTTPoison.Error{reason: reason}} ->
      {:error, reason}
  end
end
```

## 깊게 알아보기

웹 페이지 다운로드의 역사는 웹이 처음 개발된 이후로 거슬러 올라갑니다. 웹 페이지 다운로드를 통해 데이터 분석, 웹 크롤링 등 많은 방면으로 활용될 수 있습니다. 
다른 언어에서는 `wget`이나 `curl` 같은 툴을 사용해 웹 페이지를 다운로드 합니다. 
Elixir에서는 `HTTPoison` 라이브러리를 사용하는 것이 일반적입니다.

## 참고 링크

더 알아보기 위해 아래의 링크를 참고하시기 바랍니다:

- Elixir 공식 문서: https://elixir-lang.org/docs.html 
- HTTPoison GitHub: https://github.com/edgurgel/httpoison 
- 웹 크롤링에 대해: https://en.wikipedia.org/wiki/Web_scraping