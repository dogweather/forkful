---
title:                "웹 페이지 다운로드"
html_title:           "C#: 웹 페이지 다운로드"
simple_title:         "웹 페이지 다운로드"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## 왜

웹 페이지를 다운로드하는 행동을 하게 되는 이유는 여러 가지가 있을 수 있습니다. 일부 사람들은 오프라인 상태에서도 웹 페이지를 볼 수 있게 하기 위해서이고, 다른 사람들은 웹 페이지를 분석하거나 데이터를 수집하기 위해서일 수도 있습니다.

## 다운로드 방법

### 기본적인 다운로드

먼저 필요한 네임스페이스를 다음과 같이 추가합니다. ```using System.Net;```

그리고 다음의 코드를 사용하여 웹 페이지를 다운로드합니다.

```C#
WebClient client = new WebClient();
string downloadedPage = client.DownloadString("https://example.com");
Console.WriteLine(downloadedPage);
```

위 코드의 실행 결과는 다음과 같습니다.

```
<html>
  <head>
    <title>Example Domain</title>

    <meta charset="utf-8" />
    <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <style type="text/css">
      body {
        background-color: #f0f0f2;
        margin: 0;
        padding: 0;
        font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
        
        ...

      </style>    
  </head>

  <body>
    <div>
      <h1>Example Domain</h1>
      <p>This domain is for use in illustrative examples in documents. You may use this
        domain in literature without prior coordination or asking for permission.</p>
      <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </div>
  </body>
</html>
```

### 로그인이 필요한 페이지 다운로드

만약 로그인이 필요한 웹 페이지를 다운로드하려면 어떻게 해야 할까요? 이 경우에는 다른 방법을 사용해야 합니다.

먼저 로그인할 때 사용하는 양식의 데이터를 가져와야 합니다. 그리고 다음과 같이 코드를 작성합니다.

```C#
WebClient client = new WebClient();
string loginFormUrl = "https://example.com/login";
LoginFormData formData = new LoginFormData();
formData.Username = "username";
formData.Password = "password";

// 로그인에 대한 POST 요청을 생성합니다.
string postData = "Username=" + formData.Username + "&Password=" + formData.Password;

// 로그인에 대한 응답 결과를 가져옵니다.
string loginResult = client.UploadString(loginFormUrl, "POST", postData);

// 로그인이 성공하면 페이지를 다운로드합니다.
string downloadedPage = client.DownloadString("https://example.com/members_only");
Console.WriteLine(downloadedPage);
```

위 코드에서는 ```LoginFormData```라는 별도의 클래스를 사용하였는데, 이 클래스는 다음과 같이 정의될 수 있습니다.

```C#
public class LoginFormData
{
    public string Username { get; set; }
    public string Password { get; set; }
}
```

## 깊이 들어가기

여러분은 아마도 다운로드한 웹 페이지의 내용을 분석하거나 다른 데이터를 추출하고 싶을 것입니다. 이를 위해서는 다운로드한 페이지의 내용을 원하는 방식으로 파싱해야 합니다. 이를 위해 C#에서는 강력한 HTML 파싱 라이브러리인 HtmlAgilityPack을 사용할 수 있습니다.

예를 들어, 웹 페이지에서 모든 제목을 추출한다고 가정해 봅시다. 다음과 같이 코드를 작성할 수 있습니다.

```C#
// 다운로드한 페이지를 파싱합니다.
HtmlDocument doc = new HtmlDocument();
doc.LoadHtml(downloadedPage);

// 모든 h1 태그를 찾습니다.
HtmlNodeCollection allHeaders = doc.DocumentNode.SelectNode("//h1");

foreach (HtmlNode header in allHeaders)
{
    // 제목을 출력합니다.
    Console