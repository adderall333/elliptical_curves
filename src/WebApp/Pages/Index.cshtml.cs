using Library;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.Extensions.Logging;

namespace WebApp.Pages
{
    public class IndexModel : PageModel
    {
        private readonly ILogger<IndexModel> _logger;

        public IndexModel(ILogger<IndexModel> logger)
        {
            _logger = logger;
        }

        public string Message { get; set; }
        public string Result { get; set; }
        
        public void OnGet()
        {
        }
        
        public ActionResult OnPostResult(int p, int a, int b)
        {
            if (p < 2)
            {
                Message = "Argument p was too small";
                return Page();
            }

            if (a == 0 || b == 0)
            {
                Message = "Arguments a and b dont't have to be 0";
                return Page();
            }

            Result = EllipticalCurves.getResult(p, a, b);
            return Page();
        }
    }
}