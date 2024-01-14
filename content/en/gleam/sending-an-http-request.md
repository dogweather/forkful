---
title:                "Gleam recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a fundamental aspect of web development. It allows you to retrieve data from other servers and integrate it into your own application. Whether you are building a website, a mobile app, or an API, understanding how to send HTTP requests is essential for creating dynamic and interactive experiences for your users. In this blog post, we will explore how to send HTTP requests using Gleam and how it can benefit your programming projects.

## How To

To send an HTTP request in Gleam, we can use the `httpc` package. First, we need to import it into our module:

```
import httpc
```

Next, we can use the `Request` record type from the `httpc` package to create an HTTP request. Let's say we want to send a GET request to the GitHub API to retrieve information about a specific user. We can define our request like this:

```
let request =
  Request.get("https://api.github.com/users/github")
```

Notice how we specify the URL for the request as an argument to the `get()` function. Next, we can use the `send()` function to actually send the request:

```
let response = httpc.send(request)
```

The `send()` function returns a `Response` record type, which contains information about the server's response to our request. We can access the response body by using the `body()` function:

```
let body = response.body
```

Finally, we can print the response body to see the data we have received from the GitHub API:

```
IO.print(body)
```

If we run our code, we should see the following output in our terminal:

```
{"login":"github","id":9919,"node_id":"MDQ6VXNlcjI2MjM3MQ==","avatar_url":"https://avatars0.githubusercontent.com/u/9919?v=4","gravatar_id":"","url":"https://api.github.com/users/github","html_url":"https://github.com/github","followers_url":"https://api.github.com/users/github/followers","following_url":"https://api.github.com/users/github/following{/other_user}","gists_url":"https://api.github.com/users/github/gists{/gist_id}","starred_url":"https://api.github.com/users/github/starred{/owner}{/repo}","subscriptions_url":"https://api.github.com/users/github/subscriptions","organizations_url":"https://api.github.com/users/github/orgs","repos_url":"https://api.github.com/users/github/repos","events_url":"https://api.github.com/users/github/events{/privacy}","received_events_url":"https://api.github.com/users/github/received_events","type":"Organization","site_admin":false,"name":"GitHub","company":null,"blog":"","location":null,"email":null,"hireable":null,"bio":null,"twitter_username":null,"public_repos":334,"public_gists":0,"followers":0,"following":0,"created_at":"2008-05-11T04:37:31Z","updated_at":"2020-08-16T09:01:44Z"}
```

And that's how we can send an HTTP request in Gleam and retrieve data from a server. You can use this same method to make any type of HTTP request, such as POST, PUT, or DELETE, and receive a response from the server.

## Deep Dive

Under the hood, Gleam uses the `hackney` library to handle HTTP requests and responses. `hackney` is a robust HTTP client built for Erlang and provides functionality for handling redirects, authentication, and SSL certificates. By using `httpc` in Gleam, we can take advantage of all the features `hackney` has to offer without having to write low-level Erlang code.

Additionally, `httpc` handles IO operations asynchronously, which means it will not block the execution of other code while waiting for a response from the server. This allows for faster and more efficient communication with external services.

## See Also

If you want to learn more about sending HTTP requests in Gleam, check out these useful resources:

- [The `httpc` package documentation](https://gleam.run/packages/httpc/)
- [The `hackney` library documentation](https://hackney.readme.io/docs)
- [An introduction to web development with Gleam](https://dev.to/gleam_lantern/introduction-to-web-development-with-gleam-3on6)

Now that you have a better understanding of how to send HTTP requests in Gleam, you can start building powerful and dynamic applications with ease. Happy coding!