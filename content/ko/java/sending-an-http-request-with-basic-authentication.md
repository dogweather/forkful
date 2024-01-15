---
title:                "기본 인증과 함께 http 요청 보내기"
html_title:           "Java: 기본 인증과 함께 http 요청 보내기"
simple_title:         "기본 인증과 함께 http 요청 보내기"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## 왜
HTTP 요청에 기본 인증을 사용하는 이유는 서버에 안전하게 사용자를 확인할 수 있기 때문입니다. 인증 정보가 제대로 포함된 요청만이 서버에서 처리되어야 하며, 그렇지 않은 경우에는 오류가 발생해야 합니다.

## 하는 법
우선, `HttpURLConnection` 클래스를 사용해 URL에 연결합니다. 다음으로, `setRequestProperty` 메서드를 사용해 `Authorization` 헤더에 인증 정보를 추가합니다. 예시 코드는 아래와 같습니다.

```Java
URL url = new URL("http://www.example.com/api/users");
HttpURLConnection con = (HttpURLConnection) url.openConnection();

String auth = "username:password";
String encodedAuth = Base64.getEncoder().encodeToString(auth.getBytes());
String authHeaderValue = "Basic " + encodedAuth;

con.setRequestMethod("GET");
con.setRequestProperty("Authorization", authHeaderValue);

int responseCode = con.getResponseCode();

BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();

while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

System.out.println(response.toString());
```

위 코드에서는 `username`과 `password`를 `:`로 구분한 뒤, Base64로 인코딩하여 `Authorization` 헤더에 추가하고 있습니다. 이렇게 보내진 요청은 서버에서 인증 정보를 확인하고, 유효한 경우에만 요청을 처리하게 됩니다.

## 깊게 들어가기
HTTP 요청에 기본 인증을 사용하면 인증 정보가 암호화되지 않은 채 전송되기 때문에 보안상 취약점을 가지고 있습니다. 따라서 HTTPS와 같은 보안 프로토콜을 사용하거나, 인증 정보를 HTTPS보다 안전한 암호화 알고리즘으로 인코딩하는 것이 좋습니다.

또한, 위 예시 코드에서는 간단하게 인증 정보를 하드코딩하여 사용하고 있지만, 실제로는 각 사용자마다 별도의 인증 정보를 생성하고 관리해야 합니다. 사용자의 비밀번호를 안전하게 저장하고 관리하는 것이 중요합니다.

## 참고 자료
- [Java HTTP Request Example](https://www.baeldung.com/java-http-request)
- [Java Basic Authentication Example](https://www.baeldung.com/java-http-request-basic-authentication)