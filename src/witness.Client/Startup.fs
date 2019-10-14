namespace witness.Client

open Microsoft.AspNetCore.Blazor.Hosting
open Microsoft.AspNetCore.Components.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Bolero.Remoting.Client

type Startup() =

    member __.ConfigureServices(services: IServiceCollection) =
        services
          .AddRemoting()
          .AddSingleton<IJSRuntime>(new Mono.WebAssembly.Interop.MonoWebAssemblyJSRuntime())

    member __.Configure(app: IComponentsApplicationBuilder) =
        app.AddComponent<Main.MyApp>("#main")

module Program =

    [<EntryPoint>]
    let Main args =
        BlazorWebAssemblyHost.CreateDefaultBuilder()
            .UseBlazorStartup<Startup>()
            .Build()
            .Run()
        0
