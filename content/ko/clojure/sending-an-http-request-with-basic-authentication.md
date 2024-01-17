---
title:                "기본 인증으로 http 요청 보내기"
html_title:           "Clojure: 기본 인증으로 http 요청 보내기"
simple_title:         "기본 인증으로 http 요청 보내기"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 무엇인가?

HTTP 요청에 기본 인증을 첨부하는 것은 인증 정보가 없는 공용 네트워크를 통해 클라이언트와 서버 사이에 프라이빗 통신을 가능하게 하는 방법입니다. 프로그래머는 이 방법을 사용하여 사용자 이름과 비밀번호를 입력하는 대신 자동으로 데이터를 가져오고 보안된 연결을 설정할 수 있습니다.

## 방법:

`Clojure` 블록 내에서 코딩 예제와 샘플 출력을 표시합니다. 예를 들어:

```Clojure
(require '[clj-http.client :as client])

(defn get-data []
  (let [username "username"
        password "password"
        auth (str username ":" password)
        url "http://www.example.com"
        request {:as :json :basic-auth [username password]}
        response (client/get url request)]
    (println (:body response))))
```

샘플 출력:

```Clojure
{:status 200,
 :headers {"Content-Type" "application/json"},
 :body "{\"data\": \"example\"}",
 :trace-redirects ["http://www.example.com/"]}
```

## 심층 탐구:

(1) 이 방법의 역사적인 배경에는 기존의 취약한 인증 방식이 대체되어 보안 수준이 높아진 이유가 있습니다. (2) 기본 인증 이외에도 기본 인증보다 더 강력한 인증 방식을 사용할 수 있습니다. (3) HTTP 요청을 보내는 방법은 `clj-http` 라이브러리 외에도 여러 가지가 있습니다. 예를 들어, `Java` 코드를 사용하여 직접 수행할 수 있습니다.

## 관련 사이트:

- [Clojure 공식 홈페이지](https://clojure.org/)
- [clj-http 라이브러리](https://github.com/dakrone/clj-http)
- [HTTP 기본 인증에 관한 자세한 정보](https://en.wikipedia.org/wiki/Basic_access_authentication)