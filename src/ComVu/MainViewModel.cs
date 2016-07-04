using Reactive.Bindings;
using System.Reactive.Linq;

namespace ComVu
{
    public class MainViewModel
    {
        public ReactiveProperty<string> Code { get; set; }
        public ReactiveProperty<string> Output { get; set; }

        public MainViewModel()
        {
            Code = new ReactiveProperty<string>();
            Output = Code
                .ToReactiveProperty();
        }
    }
}
