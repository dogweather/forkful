---
title:                "Elixir: 发送一个http请求"
simple_title:         "发送一个http请求"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## 为什么

为了与其他应用程序或网站通信，发送 HTTP 请求是必不可少的。这可以让您的应用从其他来源获取数据，也可以将数据发送到其他应用程序。

## 如何发送 HTTP 请求

要发送 HTTP 请求，您需要使用 Elixir 中的 `HTTPoison` 库。首先，您需要在您的项目中添加它，可以在终端中使用以下命令：

```
mix deps.get
```

然后，在您的代码中导入 `HTTPoison`：

```
require HTTPoison
```

接下来，您可以使用 `HTTPoison.get/3` 函数发送 GET 请求：

```
response = HTTPoison.get("https://example.com")
```

您可以在此处替换您想要发送请求的网址。在这个例子中，`response` 变量将包含来自该网站的响应。

要发送 POST 请求，您可以使用 `HTTPoison.post/4` 函数：

```
body = %{username: "johnsmith", password: "secret"}
response = HTTPoison.post("https://example.com/login", body, [{"ContentType", "application/json"}])
```

这里，我们传递了一个 `body` 参数，它是一个包含 JSON 数据的 Elixir Map。我们还在请求头中传递了一个 `ContentType`，以告知服务器我们发送的是 JSON 数据。

## 深入了解发送 HTTP 请求

当您发送 HTTP 请求时，服务器将以响应的形式返回数据。这可能是一个成功的响应，也可能是一个错误响应。您可以使用 `HTTPoison` 库来处理这些不同的响应。

例如，您可以通过访问 `response` 变量中的 `status_code` 动态获取响应状态码。如果响应成功，状态码将为 `200`。如果响应错误，则可能为 `400` 或 `500` 等。

您也可以使用 `response` 变量中的 `body` 动态来获取返回的数据。如果服务器返回的是 JSON 数据，您可以使用 `Jason` 库来解析它。

如您所见，通过使用 `HTTPoison` 库，发送 HTTP 请求变得简单而直观。

## 另请参阅

- [HTTPoison 官方文档](https://hexdocs.pm/httpoison/1.6.1/overview.html)
- [Elixir 官方文档](https://elixir-lang.org/docs.html)
- [Jason 官方文档](https://hexdocs.pm/jason/readme.html)