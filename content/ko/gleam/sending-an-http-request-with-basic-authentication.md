---
title:                "기본 인증을 사용하여 http 요청 보내기"
html_title:           "Gleam: 기본 인증을 사용하여 http 요청 보내기"
simple_title:         "기본 인증을 사용하여 http 요청 보내기"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청을 기본 인증으로 보내는 것에 참여하는 이유는, 인증 정보를 보호하고 신뢰할 수 있는 사용자만이 액세스할 수 있는 서비스를 제공하기 위해서입니다.

## 방법
```Gleam
import gleam/http
import gleam/json
import gleam/encoding

// 인증 정보를 세팅하는 함수
fn set_auth(auth) {
  {auth.username, auth.password} =>
    { basic_user := base64.encode(auth.username)
    , basic_pass := base64.encode(auth.password)
    , basic_auth := string.concat([basic_user, ":", basic_pass])
    , headers := [{ "Authorization", "Basic " <> basic_auth }]
    }
}

// API 서버에 GET 요청을 보내는 함수, 인증 정보를 인수로 받음
fn get_data(auth) {
  set_auth(auth) =>
    let
      { basic_auth, headers } => basic_auth
      url = "https://api.example.com/data"
    in
      headers
      |> http.get(url)
      |> Result.and_then(|response| {
          let
            json = response.body |> encoding.decode_bytes
          in
            response
            |> http.response(json)
            |> Result.to_pretty |> json.encode
        })
}

fn main() {
  // API 서버의 인증 정보
  auth = { username: "user1", password: "password123" }
  get_data(auth) =>
    // 성공적으로 요청을 보내면, 서버에서 보내는 데이터를 Gleam 타입으로 변환
    // 예를 들어, [{"id": 12345, "name": "John Doe"}]
  { data1, data2 } => data1
    // {basic_username: basic_password: "username:password", headers: {"Authorization", "Basic dXNlcjE6cGFzc3dvcmQxMjM="}}
    #=> [{id: 12345, name: "John Doe"}]
}
```

## 딥 다이브
기본 인증은 HTTP 요청의 헤더에 인코딩된 사용자 이름과 비밀번호를 추가함으로써 인증이 이루어집니다. 이러한 인증 정보는 보안 측면에서 안전하지 않기 때문에 HTTPS와 같은 보안 프로토콜을 사용하여 데이터를 보호해야 합니다.

## 참고
- [Gleam HTTP 모듈 문서](https://gleam.run/modules/http)
- [Gleam JSON 모듈 문서](https://gleam.run/modules/json)
- [온라인 Base64 인코더](https://www.base64encode.org/)